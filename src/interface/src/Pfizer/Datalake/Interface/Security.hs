{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Pfizer.Datalake.Interface.Security (
    -- * Passwords
    PlaintextPassword(..)
    -- * Groups
  , GroupIx(..)
  , GroupName
    -- * Authentication tokens
  , AuthToken(..)
    -- * Permissions
  , DatasetAccessLevel(..)
  ) where

import Control.Monad
import Data.Aeson hiding (Value(..))
import Data.String
import GHC.Generics (Generic)
import Text.Show.Pretty
import qualified Data.ByteString       as BS.S
import qualified Data.ByteString.Char8 as BS.S.C8

import Pfizer.Datalake.Interface.DB
import Pfizer.Datalake.Interface.Pretty

{-------------------------------------------------------------------------------
  Passwords
-------------------------------------------------------------------------------}

-- | Plaintext password (only used as part of user input)
newtype PlaintextPassword = PlaintextPassword String
  deriving (Show, Eq, Ord, IsString, PrettyVal)

{-------------------------------------------------------------------------------
  Groups
-------------------------------------------------------------------------------}

newtype GroupIx = GroupIx Ix
  deriving (Show, Pretty)

type GroupName = String

{-------------------------------------------------------------------------------
  Authentication tokens
-------------------------------------------------------------------------------}

-- | Authentication token
--
-- A login token can be requested once the user has identified themselves
-- using their username and password. The token can then be stored in a
-- file and used on subsequent requests to resume the session.
--
-- Tokens are UUID strings so we don't need to worry about unicode encoding.
--
-- This intentionally does not derive 'FromHttpApiData' or 'ToHttpApiData';
-- these tokens should not appear in URLs.
newtype AuthToken = AuthToken BS.S.ByteString
  deriving (Show, Pretty)

instance FromJSON AuthToken where
  parseJSON = liftM (AuthToken . BS.S.C8.pack) . parseJSON

instance ToJSON AuthToken where
  toJSON (AuthToken bs) = toJSON (BS.S.C8.unpack bs)

instance PrettyVal AuthToken where
  prettyVal (AuthToken bs) = Con "AuthToken" [String (BS.S.C8.unpack bs)]

{-------------------------------------------------------------------------------
  Permissions
-------------------------------------------------------------------------------}

-- | Dataset access level
--
-- The order of the constructors is important, as it determines the 'Ord'
-- instance; 'maximum' should have the intended meaning (maximum access).
data DatasetAccessLevel =
    DatasetAccessLevelNone
  | DatasetAccessLevelRead
  | DatasetAccessLevelUpdate
  | DatasetAccessLevelManage
  deriving (Show, Read, Eq, Ord, Generic)

instance PrettyVal DatasetAccessLevel

instance Pretty DatasetAccessLevel where
  pretty DatasetAccessLevelNone   = "no access"
  pretty DatasetAccessLevelRead   = "READ"
  pretty DatasetAccessLevelUpdate = "UPDATE"
  pretty DatasetAccessLevelManage = "MANAGE"

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

instance ToJSON DatasetAccessLevel where
  toJSON DatasetAccessLevelNone   = "none"
  toJSON DatasetAccessLevelRead   = "read"
  toJSON DatasetAccessLevelUpdate = "update"
  toJSON DatasetAccessLevelManage = "manage"

instance FromJSON DatasetAccessLevel where
  parseJSON = withText "DatasetAccessLevel" $ \str ->
    case str of
      "none"     -> return DatasetAccessLevelNone
      "read"     -> return DatasetAccessLevelRead
      "update"   -> return DatasetAccessLevelUpdate
      "manage"   -> return DatasetAccessLevelManage
      _otherwise -> fail "Could not parse DatasetAccessLevel"
