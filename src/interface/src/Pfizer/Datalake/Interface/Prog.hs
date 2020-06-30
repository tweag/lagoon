-- Copyright 2020 Pfizer Inc.

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     https://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
{-# LANGUAGE OverloadedStrings #-}
module Pfizer.Datalake.Interface.Prog (
    -- * Prog EDSL
    ClosedProg_(..)
  , ClosedProg
  , Prog(..)
  , ManageDataset(..)
  , ManageGroup(..)
  , ManageUser(..)
  , Done(..)
  , Output(..)
    -- * Helpers
  , done
  ) where

import Data.Functor.Identity
import Data.Monoid
import Data.Typeable
import GHC.Generics (Generic)
import Text.Show.Pretty
import qualified Text.PrettyPrint as PP

import Pfizer.Datalake.Interface

{-------------------------------------------------------------------------------
  Prog EDSL
-------------------------------------------------------------------------------}

type ClosedProg = ClosedProg_ Done
newtype ClosedProg_ a = ClosedProg (forall f. Applicative f => Prog f (f a))

-- | Abstract definition of the DB API
--
-- We parameterize over the type of variables (@f@) so that we can
-- define a 'PrettyVal' instance for 'Prog' even though we have embedded
-- functions (PHOAS inspired representation).
data Prog f a =
    -- | Done (printing a final result to the user)
    Pretty (Output f) a

    -- | Login
    --
    -- We either get a set of credentials of a filepath to a file containing
    -- an 'AuthToken'. As for some other 'Prog' constructors this is
    -- somewhat client-biased.
  | Login (Either Credentials FilePath) (Prog f a)

    -- | Login as administrator
  | LoginAdmin PlaintextPassword (Prog f a)

    -- | Logout
  | Logout (Prog f a)

    -- | Get an auth token and store it in the given file
  | GetAuthToken FilePath (Prog f a)

    -- | Get ID of existing source name (error if not found)
  | GetSourceName SourceName (f SourceNameIx -> Prog f a)

    -- | Get ID of a previously ingested table (error if not found)
  | GetVersion (f SourceNameIx) (Maybe Version) (f SourceIx -> Prog f a)

    -- | Get the IDs of all the versions ingested under that source name
  | GetAllVersions (f SourceNameIx) (f [SourceIx] -> Prog f a)

    -- | Get info about a specific column in table (error if not found)
  | GetColumn (f SourceIx) ColumnName (f ColumnIx -> Prog f a)

    -- | Get information about a single source
  | GetSourceInfo (f SourceIx) (f SourceInfo -> Prog f a)

    -- | List available sources
  | GetSources SourcesSpec (f Sources -> Prog f a)

    -- | Ingest a datasource
    --
    -- The specification is somewhat client-biased here: for uploaded files we
    -- specify the (client-local) 'FilePath' of the file. Ideally we'd remove
    -- that bias but right now I don't see an easy way to do that.
  | Ingest SourceName IngestOptions CreateOptions CreateIndices IngestPrivate (Input FilePath) (f SourceInfo -> Prog f a)

    -- | Construct typed table
  | MakeTyped (f SourceIx) CreateIndices (f SourceInfo -> Prog f a)

    -- | Override type specification
  | SetColumnType (f ColumnIx) ColumnType (Prog f a)

    -- | Manage a dataset
  | ManageDataset (f SourceNameIx) (f SourceIx) [ManageDataset] (Prog f a)

    -- | Manage a group
  | ManageGroup GroupName [ManageGroup] (Prog f a)

    -- | Manage users
  | ManageUser [ManageUser] (Prog f a)

    -- | Create a new group
  | CreateGroup GroupName (Prog f a)

    -- | Tag a source
  | TagSource (f SourceIx) TagName (Prog f a)

    -- | Untag a source
  | UntagSource (f SourceIx) TagName (Prog f a)

    -- | JSON type inference
    --
    -- The same client-side bias that appears in 'Ingest' appears here too.
  | InferJsonType (Maybe DecompressMethod) (Input FilePath) (f JsonType -> Prog f a)

    -- | Download a previously ingested source
  | DownloadSource (f SourceIx) (Prog f a)

    -- | Delete a previously ingested source
  | DeleteSources (f [SourceIx]) (Prog f a)

  | Compact SourceName CreateOptions CreateIndices IngestPrivate (f [SourceIx]) (f SourceInfo -> Prog f a)
  deriving (Generic, Functor)

-- | Manage dataset
data ManageDataset =
    SetDeprecated Bool
  | SetPublic Bool
  | SetGroupAccessLevel GroupName DatasetAccessLevel
  | SetUserAccessLevel  UserName DatasetAccessLevel
  deriving (Generic)

-- | Manage a group
data ManageGroup =
    AddUser UserName
  | RemoveUser UserName
  | GrantManageGroup UserName
  | RevokeManageGroup UserName
  deriving (Generic)

-- | Manage a user (only the DB admin can do this)
data ManageUser =
    GrantCreateSource  UserName
  | RevokeCreateSource UserName
  | GrantCreateGroup   UserName
  | RevokeCreateGroup  UserName
  | CreateUser         UserName
  deriving (Generic)

-- | Output to the user
data Output f =
    forall a. Output (a -> Doc) (f a)
  | SimpleOutput Doc
  | NoOutput

-- | Indicate we are done
--
-- We're using this instead of @()@ to avoid the need for a 'PrettyVal' orphan
data Done = Done deriving (Generic)

done :: forall f. Applicative f => f Done
done = pure Done

{-------------------------------------------------------------------------------
  Pretty-printing (used for user output in command line)
-------------------------------------------------------------------------------}

-- | We make sure to add a newline in the rendered string version,
-- but only if the document is not empty.
instance Pretty (Output Identity) where
  pretty (Output f (Identity a)) = f a
  pretty (SimpleOutput doc)      = doc
  pretty NoOutput                = mempty

  prettyStr output =
    let doc = pretty output
    in if PP.isEmpty doc then ""
                         else PP.render doc ++ "\n"

instance Pretty ManageDataset where
  pretty (SetDeprecated True)  = "mark as deprecated"
  pretty (SetDeprecated False) = "unmask as deprecated"
  pretty (SetPublic True)      = "make public"
  pretty (SetPublic False)     = "make private"
  pretty (SetGroupAccessLevel group level) = PP.hcat [
        "set access level of group "
      , pretty group
      , " to level "
      , pretty level
      ]
  pretty (SetUserAccessLevel user level) = PP.hcat [
        "set access level of user "
      , pretty user
      , " to level "
      , pretty level
      ]

instance Pretty ManageGroup where
  pretty (AddUser           user) = "add user "    <> pretty user
  pretty (RemoveUser        user) = "remove user " <> pretty user
  pretty (GrantManageGroup  user) = "grant MANAGEGROUP to " <> pretty user
  pretty (RevokeManageGroup user) = "revoke MANAGEGROUP from " <> pretty user

instance Pretty ManageUser where
  pretty (GrantCreateSource  user) = "grant CREATE to "         <> pretty user
  pretty (RevokeCreateSource user) = "revoke CREATE from "      <> pretty user
  pretty (GrantCreateGroup   user) = "grant CREATEGROUP to "    <> pretty user
  pretty (RevokeCreateGroup  user) = "revoke CREATEGROUP from " <> pretty user
  pretty (CreateUser         user) = "create user " <> pretty user

{-------------------------------------------------------------------------------
  PrettyVal for 'Prog' (for debugging)
-------------------------------------------------------------------------------}

data MetaVar f = MetaVar

instance Functor MetaVar where
  fmap _ MetaVar = MetaVar

instance Applicative MetaVar where
  pure _  = MetaVar
  _ <*> _ = MetaVar

instance Typeable f => PrettyVal (MetaVar f) where
  prettyVal MetaVar = String $ "<<" ++ show (typeOf (undefined :: f)) ++ ">>"

instance PrettyVal a => PrettyVal (MetaVar f -> Prog MetaVar a) where
  prettyVal = prettyVal . ($ MetaVar)

instance PrettyVal a => PrettyVal (Prog MetaVar a) where
  -- use Generic derivation

instance (Typeable a, PrettyVal a) => PrettyVal (ClosedProg_ a) where
  prettyVal (ClosedProg prog) = prettyVal (prog :: Prog MetaVar (MetaVar a))

instance PrettyVal (Output f) where
  prettyVal _ = String "<<Output>>"

instance PrettyVal Done
instance PrettyVal ManageDataset
instance PrettyVal ManageGroup
instance PrettyVal ManageUser
