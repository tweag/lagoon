-- | Server API
--
-- Intended for qualified import
--
-- > import qualified Pfizer.Datalake.Interface.API as API
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pfizer.Datalake.Interface.API (
    Datalake
    -- * Datalake REST API
  , proxy
    -- ** /source
  , Source
  , SourceGet
  , SourceDelete
  , SourceTagsPost
  , SourceTagDelete
  , SourceDownload
  , SourceGetColumn
  , SourceMakeTyped
  , SourceSetDeprecated
  , SourceSetPublic
  , SourceSetGroupAccessLevel
  , SourceSetUserAccessLevel
    -- ** /source without a known 'SourceIx'
  , SourceByName
  , SourceWithVersion
  , SourceAllVersions
  , SourceInferJsonType
    -- ** /sources
  , Sources
  , SourcesGet
  , SourcesPost
  , SourcesCompact
    -- ** /column
  , Column
  , ColumnSetType
    -- ** /sql
  , SQL
    -- ** /user
  , User
  , UserLogin
  , UserLogout
  , UserGetAuthToken
  , UserResumeSession
  , UserSetCanCreateSource
  , UserSetCanCreateGroup
    -- ** /users
  , Users
  , UsersCreate
    -- ** /group
  , Group
  , GroupAddUser
  , GroupRemoveUser
  , GroupAddAdmin
  , GroupRemoveAdmin
    -- ** /groups
  , Groups
  , GroupsCreate
    -- ** /debug
  , Debug
  , DebugDumpDbInfo
  , DebugRebuildCanReadCache
    -- * Compound query parameters
  , ExpandInput
  , ExpandSourcesSpec
  , ExpandIngestOptions
  , ExpandCreateOptions
    -- * Servant support
  , Session
  , StreamResponse
  , Data
  , AcceptHeader(..)
  , WithCookie
  , QueryParamEither
  , QueryParamsEither
  ) where

import Data.Either (rights)
import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid
import Data.Proxy
import Data.Typeable
import GHC.TypeLits
import Network.URI
import Servant.API
import qualified Data.ByteString       as BS.S
import qualified Data.ByteString.Char8 as BS.S.C8
import qualified Data.ByteString.UTF8  as BS.S.UTF8
import qualified Data.Text             as Text

import qualified Pfizer.Datalake.Interface as I

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

type Datalake =
         "source"  :> Source
    :<|> "sources" :> Sources
    :<|> "column"  :> Column
    :<|> "sql"     :> SQL
    :<|> "user"    :> User
    :<|> "users"   :> Users
    :<|> "group"   :> Group
    :<|> "groups"  :> Groups
    :<|> "debug"   :> Debug

proxy :: Proxy Datalake
proxy = Proxy

{-------------------------------------------------------------------------------
  /source

  NOTE: Most of these have the shape

  > /source/:SourceIx/...

  where the 'SourceIx' comes first (e.g. @/source/:SourceIx/download@).
  We need to make sure to keep this structure consistent. The only exceptions
  are when we are not sure about the 'SourceIx':

  > /source/name/:SourceName
  > /source/version/:SourceNameIx [?version=:Version]
-------------------------------------------------------------------------------}

type Source =
         SourceGet
    :<|> SourceDelete
    :<|> SourceTagsPost
    :<|> SourceTagDelete
    :<|> SourceDownload
    :<|> SourceGetColumn
    :<|> SourceMakeTyped
    :<|> SourceSetDeprecated
    :<|> SourceSetPublic
    :<|> SourceSetGroupAccessLevel
    :<|> SourceSetUserAccessLevel
    :<|> SourceByName
    :<|> SourceWithVersion
    :<|> SourceAllVersions
    :<|> SourceInferJsonType

type SourceGet =
       Session
    :> Capture "SourceIx" I.SourceIx
    :> Get '[JSON] I.SourceInfo

type SourceDelete =
       Session
    :> Capture "SourceIx" I.SourceIx
    :> Delete '[JSON] NoContent

type SourceTagsPost =
       Session
    :> Capture "SourceIx" I.SourceIx
    :> "tags"
    :> ReqBody '[JSON] [I.TagName]
    :> Post '[JSON] NoContent

type SourceTagDelete =
       Session
    :> Capture "SourceIx" I.SourceIx
    :> "tag"
    :> Capture "TagName" I.TagName
    :> Delete '[JSON] NoContent

type SourceDownload =
       Session
    :> Capture "SourceIx" I.SourceIx
    :> "download"
    :> StreamResponse Data 'GET

type SourceGetColumn =
       Session
    :> Capture "SourceIx" I.SourceIx
    :> "column"
    :> Capture "ColumnName" I.ColumnName
    :> Get '[JSON] I.ColumnIx

type SourceMakeTyped =
       Session
    :> Capture "SourceIx" I.SourceIx
    :> QueryFlag "noIndices"
    :> StreamResponse (I.ProgressOr I.SourceInfo) 'POST

type SourceSetDeprecated =
       Session
    :> Capture "SourceIx" I.SourceIx
    :> "deprecated"
    :> ReqBody '[JSON] Bool
    :> Post '[JSON] NoContent

-- Permissions
-- TODO: Should these live under a different prefix than /source?
-- After all, they use SourceNameIx rather than SourceIx

type SourceSetPublic =
       Session
    :> Capture "SourceNameIx" I.SourceNameIx
    :> "public"
    :> ReqBody '[JSON] Bool
    :> Post '[JSON] NoContent

type SourceSetGroupAccessLevel =
       Session
    :> Capture "SourceNameIx" I.SourceNameIx
    :> "groupAccessLevel"
    :> Capture "GroupName" I.GroupName
    :> ReqBody '[JSON] I.DatasetAccessLevel
    :> Post '[JSON] NoContent

type SourceSetUserAccessLevel =
       Session
    :> Capture "SourceNameIx" I.SourceNameIx
    :> "userAccessLevel"
    :> Capture "UserName" I.UserName
    :> ReqBody '[JSON] I.DatasetAccessLevel
    :> Post '[JSON] NoContent

-- SourceIx unknown:

type SourceByName =
       Session
    :> "name"
    :> Capture "SourceName" I.SourceName
    :> Get '[JSON] I.SourceNameIx

type SourceWithVersion =
       Session
    :> "version"
    :> Capture "SourceNameIx" I.SourceNameIx
    :> QueryParam "version" I.Version
    :> Get '[JSON] I.SourceIx

type SourceAllVersions =
       Session
    :> "versions"
    :> Capture "SourceNameIx" I.SourceNameIx
    :> Get '[JSON] [I.SourceIx]

type SourceInferJsonType =
       Session
    :> "inferJsonType"
    :> QueryParam "decompressMethod" I.DecompressMethod
    :> I.Input -- intentionally without argument
    :> Post '[JSON] I.JsonType

{-------------------------------------------------------------------------------
  /sources
-------------------------------------------------------------------------------}

type Sources =
         SourcesGet
    :<|> SourcesPost
    :<|> SourcesCompact

type SourcesGet =
       Session
    :> I.SourcesSpec
    :> Get '[JSON] (Headers '[Header "X-Total-Count" I.SourcesCount] I.Sources)

type SourcesPost =
       Session
    :> I.IngestOptions
    :> I.CreateOptions
    :> QueryFlag "noIndices"
    :> QueryFlag "private"
    :> QueryParam "name" I.SourceName
    :> I.Input -- intentionally without argument
    :> StreamResponse (I.ProgressOr I.SourceInfo) 'POST

type SourcesCompact =
       Session
    :> "compact"
    :> I.CreateOptions
    :> QueryFlag "noIndices"
    :> QueryFlag "private"
    :> QueryParams "sources" I.SourceIx
    :> QueryParam "name" I.SourceName
    :> StreamResponse (I.ProgressOr I.SourceInfo) 'POST

{-------------------------------------------------------------------------------
  /column

  As for source, these have the structure @/column/:ColumnIx/...@
-------------------------------------------------------------------------------}

type Column = ColumnSetType

type ColumnSetType =
       Session
    :> Capture "ColumnIx" I.ColumnIx
    :> "type"
    :> ReqBody '[JSON] I.ColumnType
    :> Post '[JSON] NoContent

{-------------------------------------------------------------------------------
  /sql
-------------------------------------------------------------------------------}

type SQL =
       Session
    :> ReqBody '[JSON] I.UserQuery
    :> Header "accept" (AcceptHeader I.CopyToFormat)
    :> StreamResponse Data 'POST

{-------------------------------------------------------------------------------
  /user
-------------------------------------------------------------------------------}

type User =
         UserLogin
    :<|> UserLogout
    :<|> UserGetAuthToken
    :<|> UserResumeSession
    :<|> UserSetCanCreateSource
    :<|> UserSetCanCreateGroup

type UserLogin =
       "login"
    :> ReqBody '[JSON] I.Credentials
    :> QueryFlag "persistent"
    :> WithCookie (Post '[JSON] (I.LoginResult I.LoginInfo))

type UserLogout =
       Session
    :> "logout"
    :> WithCookie (Post '[JSON] NoContent)

type UserGetAuthToken =
       Session
    :> "token"
    :> Post '[JSON] I.AuthToken

type UserResumeSession =
       "resume"
    :> ReqBody '[JSON] I.AuthToken
    :> WithCookie (Post '[JSON] (I.LoginResult I.LoginInfo))

type UserSetCanCreateSource =
       Session
    :> Capture "UserName" I.UserName
    :> "create"
    :> ReqBody '[JSON] Bool
    :> Post '[JSON] NoContent

type UserSetCanCreateGroup =
       Session
    :> Capture "UserName" I.UserName
    :> "creategroup"
    :> ReqBody '[JSON] Bool
    :> Post '[JSON] NoContent

{-------------------------------------------------------------------------------
  /users
-------------------------------------------------------------------------------}

type Users = UsersCreate

type UsersCreate =
     Session
  :> ReqBody '[JSON] I.UserName
  :> Post '[JSON] NoContent

{-------------------------------------------------------------------------------
  /group
-------------------------------------------------------------------------------}

type Group =
         GroupAddUser
    :<|> GroupRemoveUser
    :<|> GroupAddAdmin
    :<|> GroupRemoveAdmin

type GroupAddUser =
       Session
    :> Capture "GroupName" I.GroupName
    :> "members"
    :> ReqBody '[JSON] I.UserName
    :> Post '[JSON] NoContent

type GroupRemoveUser =
       Session
    :> Capture "GroupName" I.GroupName
    :> "member"
    :> Capture "UserName" I.UserName
    :> Delete '[JSON] NoContent

type GroupAddAdmin =
       Session
    :> Capture "GroupName" I.GroupName
    :> "admins"
    :> ReqBody '[JSON] I.UserName
    :> Post '[JSON] NoContent

type GroupRemoveAdmin =
       Session
    :> Capture "GroupName" I.GroupName
    :> "admin"
    :> Capture "UserName" I.UserName
    :> Delete '[JSON] NoContent

{-------------------------------------------------------------------------------
  /groups
-------------------------------------------------------------------------------}

type Groups = GroupsCreate

type GroupsCreate =
     Session
  :> ReqBody '[JSON] I.GroupName
  :> Post '[JSON] NoContent

{-------------------------------------------------------------------------------
  /debug
-------------------------------------------------------------------------------}

type Debug =
         "dumpDbInfo"          :> DebugDumpDbInfo
    :<|> "rebuildCanReadCache" :> DebugRebuildCanReadCache

type DebugDumpDbInfo =
       Session
    :> Get '[JSON] I.Sources

type DebugRebuildCanReadCache =
       Session
    :> Post '[JSON] NoContent

{-------------------------------------------------------------------------------
  Compound query parameters
-------------------------------------------------------------------------------}

-- | Expansion of 'Input'
type ExpandInput sub =
       QueryParamEither "input" "remote" FilePath URI
    :> sub

-- | Expansion of 'IngestOptions' into individual query parameters and flags
type ExpandIngestOptions sub =
       QueryParam  "fileType"         I.FileType
    :> QueryParam  "peekAt"           Int
    :> QueryParam  "decompressMethod" I.DecompressMethod
    :> QueryParam  "jsonPath"         I.JsonPath
    :> QueryParam  "encoding"         I.Encoding
    :> QueryParam  "sourceMetadataName"  String
    :> QueryParam  "sourceMetadataField" String
    :> QueryFlag "noHeaders"
    :> QueryFlag "disableQuoteChar"
    :> QueryFlag "noTypeInference"
    :> QueryParam "sourceIdentifier"  String
    :> sub

-- | Expansion of 'CreateOptions' into individual query parameters and flags
--
type ExpandCreateOptions sub =
       QueryParam  "description"      String
    :> QueryParam  "created"          I.Timestamp
    :> QueryParams "tag"              I.TagName
    :> QueryFlag "noTyped"
    :> QueryParam  "logEvery"         Int
    :> sub

-- | Expansion of 'SourcesSpec' into individual query parameters and flags
--
-- TODO: @search@ should be of type 'TsQuery', not 'String'.
-- TODO: @fact@ should be of type 'Fact', not 'String'.
type ExpandSourcesSpec sub =
       QueryParam  "offset"        Int
    :> QueryParam  "limit"         Int
    :> QueryParam  "search"        String
    :> QueryParam  "ix"            I.Ix
    :> QueryParams "tag"           String
    :> QueryParam  "description"   String
    :> QueryParam  "name"          String
    :> QueryParams "user"          String
    :> QueryParams "column"        String
    :> QueryParam  "createdAfter"  I.Timestamp
    :> QueryParam  "createdBefore" I.Timestamp
    :> QueryParamsEither "orderAsc" "orderDesc" I.SourcesColumn I.SourcesColumn
    :> QueryFlag   "includeDeprecated"
    :> sub

{-------------------------------------------------------------------------------
  Servant support

  Most of these types here are uninterpreted, because the server and the client
  have different interpretations.
-------------------------------------------------------------------------------}

-- | Current session (authenticated user)
data Session

-- | Server streams response to the client
data StreamResponse (response :: *) (method :: StdMethod)

-- | Generic argument to 'StreamResponse' for streaming unspecified " data "
data Data

-- | @"Accept: ..."@  header
data AcceptHeader a = AcceptHeader [a]
  deriving (Show)

-- | Server might add a cookie to the response
--
-- TODO: Perhaps it would be useful to track at the type-level _which_
-- cookie the server sets.
data WithCookie a

-- | Choose between two query parameters
data QueryParamEither (symA :: Symbol) (symB :: Symbol) a b

-- | Combination of 'QueryParams' and 'QueryParamEither': collecting parameters
-- of two names, whilst maintaining their relative order
data QueryParamsEither (symA :: Symbol) (symB :: Symbol) a b
    deriving Typeable

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

instance ToHttpApiData a => ToHttpApiData (AcceptHeader a) where
  toQueryParam (AcceptHeader xs) = "Accept: "
                                <> Text.intercalate "," (map toQueryParam xs)

-- | This never throws an exception; any formats that the client specifies
-- but we don't recognize we silently ignore.
instance FromHttpApiData a => FromHttpApiData (AcceptHeader a) where
  parseQueryParam = Right
                  . AcceptHeader
                  . rights
                  . map parseHeader
                  . parseHttpAccept
                  . BS.S.UTF8.fromString
                  . Text.unpack

{-------------------------------------------------------------------------------
  Serialization (orphans)
-------------------------------------------------------------------------------}

instance ToHttpApiData URI where
  toQueryParam = toQueryParam . show

instance FromHttpApiData URI where
  parseQueryParam txt =
    case parseURI (Text.unpack txt) of
      Just uri -> Right uri
      Nothing  -> Left "Invalid URI"

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Parse HTTP accept header
--
-- This is copied from @wai-extra@ (which we don't want to depend on in
-- the @-interface@ library).
parseHttpAccept :: BS.S.ByteString -> [BS.S.ByteString]
parseHttpAccept = map fst
                . sortBy (rcompare `on` snd)
                . map (addSpecificity . grabQ)
                . BS.S.split 44 -- comma
  where
    rcompare :: (Double,Int) -> (Double,Int) -> Ordering
    rcompare = flip compare
    addSpecificity (s, q) =
        -- Prefer higher-specificity types
        let semicolons = BS.S.count 0x3B s
            stars = BS.S.count 0x2A s
        in (s, (q, semicolons - stars))
    grabQ s =
        -- Stripping all spaces may be too harsh.
        -- Maybe just strip either side of semicolon?
        let (s', q) = BS.S.breakSubstring ";q=" (BS.S.filter (/=0x20) s) -- 0x20 is space
            q' = BS.S.takeWhile (/=0x3B) (BS.S.drop 3 q) -- 0x3B is semicolon
         in (s', readQ q')
    readQ s = case reads $ BS.S.C8.unpack s of
                (x, _):_ -> x
                _ -> 1.0
