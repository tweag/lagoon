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
module Lagoon.Client (
    -- * API clients
    -- ** /source
    sourceGet
  , sourceDelete
  , sourceTagsPost
  , sourceTagDelete
  , sourceDownload
  , sourceGetColumn
  , sourceMakeTyped
  , sourceSetDeprecated
  , sourceSetPublic
  , sourceSetGroupAccessLevel
  , sourceSetUserAccessLevel
    -- ** /source but without a known SourceIx
  , sourceByName
  , sourceWithVersion
  , sourceAllVersions
  , sourceInferJsonType
    -- ** /sources
  , sourcesGet
  , sourcesPost
  , sourcesCompact
    -- ** /column
  , columnSetType
    -- ** /sql
  , sql
    -- ** /user
  , userLogin
  , userLogout
  , userGetAuthToken
  , userResumeSession
  , userSetCanCreateSource
  , userSetCanCreateGroup
    -- ** /users
  , usersCreate
    -- ** /group
  , groupAddUser
  , groupRemoveUser
  , groupAddAdmin
  , groupRemoveAdmin
    -- ** /groups
  , groupsCreate
    -- ** /debug
  , debugDumpDbInfo
  , debugRebuildCanReadCache
  ) where

import Servant.API
import Servant.Client

import Lagoon.Interface ()
import Lagoon.Client.Serialization ()
import Lagoon.Client.Servant.Cookie ()
import Lagoon.Client.Servant.Conduit ()
import Lagoon.Client.Servant.Session ()
import qualified Lagoon.Interface.API as API

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

lagoon :: Client API.Lagoon
lagoon = client API.proxy

source  :: Client API.Source
sources :: Client API.Sources
column  :: Client API.Column
sql     :: Client API.SQL
user    :: Client API.User
users   :: Client API.Users
group   :: Client API.Group
groups  :: Client API.Groups
debug   :: Client API.Debug
(     source
 :<|> sources
 :<|> column
 :<|> sql
 :<|> user
 :<|> users
 :<|> group
 :<|> groups
 :<|> debug
 ) = lagoon

{-------------------------------------------------------------------------------
  /source
-------------------------------------------------------------------------------}

sourceGet                 :: Client API.SourceGet
sourceDelete              :: Client API.SourceDelete
sourceTagsPost            :: Client API.SourceTagsPost
sourceTagDelete           :: Client API.SourceTagDelete
sourceDownload            :: Client API.SourceDownload
sourceGetColumn           :: Client API.SourceGetColumn
sourceMakeTyped           :: Client API.SourceMakeTyped
sourceSetDeprecated       :: Client API.SourceSetDeprecated
sourceSetPublic           :: Client API.SourceSetPublic
sourceSetGroupAccessLevel :: Client API.SourceSetGroupAccessLevel
sourceSetUserAccessLevel  :: Client API.SourceSetUserAccessLevel
sourceByName              :: Client API.SourceByName
sourceWithVersion         :: Client API.SourceWithVersion
sourceAllVersions         :: Client API.SourceAllVersions
sourceInferJsonType       :: Client API.SourceInferJsonType

(     sourceGet
 :<|> sourceDelete
 :<|> sourceTagsPost
 :<|> sourceTagDelete
 :<|> sourceDownload
 :<|> sourceGetColumn
 :<|> sourceMakeTyped
 :<|> sourceSetDeprecated
 :<|> sourceSetPublic
 :<|> sourceSetGroupAccessLevel
 :<|> sourceSetUserAccessLevel
 :<|> sourceByName
 :<|> sourceWithVersion
 :<|> sourceAllVersions
 :<|> sourceInferJsonType
 ) = source

{-------------------------------------------------------------------------------
  /sources
-------------------------------------------------------------------------------}

sourcesGet  :: Client API.SourcesGet
sourcesPost :: Client API.SourcesPost
sourcesCompact :: Client API.SourcesCompact

(     sourcesGet
 :<|> sourcesPost
 :<|> sourcesCompact
 ) = sources

{-------------------------------------------------------------------------------
  /column
-------------------------------------------------------------------------------}

columnSetType :: Client API.ColumnSetType

(     columnSetType
 ) = column

{-------------------------------------------------------------------------------
  /sql
-------------------------------------------------------------------------------}

-- Nothing to do here, the /sql endpoint only provides a single function

{-------------------------------------------------------------------------------
  /usr
-------------------------------------------------------------------------------}

userLogin              :: Client API.UserLogin
userLogout             :: Client API.UserLogout
userGetAuthToken       :: Client API.UserGetAuthToken
userResumeSession      :: Client API.UserResumeSession
userSetCanCreateSource :: Client API.UserSetCanCreateSource
userSetCanCreateGroup  :: Client API.UserSetCanCreateGroup

(     userLogin
 :<|> userLogout
 :<|> userGetAuthToken
 :<|> userResumeSession
 :<|> userSetCanCreateSource
 :<|> userSetCanCreateGroup
 ) = user

{-------------------------------------------------------------------------------
  /users
-------------------------------------------------------------------------------}

usersCreate :: Client API.UsersCreate

(     usersCreate
 ) = users

{-------------------------------------------------------------------------------
  /group
-------------------------------------------------------------------------------}

groupAddUser     :: Client API.GroupAddUser
groupRemoveUser  :: Client API.GroupRemoveUser
groupAddAdmin    :: Client API.GroupAddAdmin
groupRemoveAdmin :: Client API.GroupRemoveAdmin

(     groupAddUser
 :<|> groupRemoveUser
 :<|> groupAddAdmin
 :<|> groupRemoveAdmin
 ) = group

{-------------------------------------------------------------------------------
  /groups
-------------------------------------------------------------------------------}

groupsCreate :: Client API.GroupsCreate

(     groupsCreate
 ) = groups

{-------------------------------------------------------------------------------
  /debug
-------------------------------------------------------------------------------}

debugDumpDbInfo          :: Client API.DebugDumpDbInfo
debugRebuildCanReadCache :: Client API.DebugRebuildCanReadCache

(     debugDumpDbInfo
 :<|> debugRebuildCanReadCache
 ) = debug
