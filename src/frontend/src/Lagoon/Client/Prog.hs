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
-- | REST interpreter for 'Prog'
module Lagoon.Client.Prog (
    runProg
  , IngestAbortedException(..)
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Aeson
import Data.Conduit
import Data.Functor.Identity
import Network.HTTP.Client (Response)
import Servant.API
import System.IO
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.ByteString          as BS.S
import qualified Data.ByteString.Char8    as BS.S.C8
import qualified Data.ByteString.Lazy     as BS.L
import qualified Network.HTTP.Client      as Client

import Lagoon.Client.Servant.Conduit
import Lagoon.Client.Servant.Cookie
import Lagoon.Interface
import Lagoon.Interface.Prog
import qualified Lagoon.Client as C

runProg :: GlobalCookieJar -> LogLevel -> ClosedProg_ a -> ResClientM a
runProg cookieJar logLevel (ClosedProg prog) =
    runIdentity <$> int cookieJar logLevel prog

int :: GlobalCookieJar -> LogLevel -> Prog Identity a -> ResClientM a
int cookieJar logLevel = go
  where
    go :: Prog Identity a -> ResClientM a
    go (Pretty output a) = liftIO $ do
        when (logLevel <= Notice) $ putStr $ prettyStr output
        return a

    go (Login (Left creds) k) = do
      let persistent = False
      _loginInfo <- lift $ fromLoginOk =<< C.userLogin creds persistent cookieJar
      go $ k

    go (Login (Right fp) k) = do
      -- We allow for a newline character
      let mkToken :: BS.S.ByteString -> AuthToken
          mkToken = AuthToken . head . BS.S.C8.lines
      token <- liftIO $ mkToken <$> BS.S.readFile fp
      _loginInfo <- lift $ fromLoginOk =<< C.userResumeSession token cookieJar
      go $ k

    go (LoginAdmin (PlaintextPassword pw) k) = do
      go $ Login (Left (Credentials "admin" pw)) k

    go (Logout k) = do
      NoContent <- lift $ C.userLogout cookieJar
      go $ k

    go (GetAuthToken fp k) = do
      AuthToken token <- lift $ C.userGetAuthToken
      liftIO $ BS.S.writeFile fp token
      go $ k

    go (GetSourceName name k) = do
      sourceNameIx <- lift $ C.sourceByName name
      go $ k (Identity sourceNameIx)

    go (GetVersion (Identity sourceNameIx) mVersion k) = do
      sourceIx <- lift $ C.sourceWithVersion sourceNameIx mVersion
      go $ k (Identity sourceIx)

    go (GetAllVersions (Identity sourceNameIx) k) = do
      sourceIxs <- lift $ C.sourceAllVersions sourceNameIx
      go $ k (Identity sourceIxs)

    go (GetColumn (Identity sourceIx) name k) = do
      columnIx <- lift $ C.sourceGetColumn sourceIx name
      go $ k (Identity columnIx)

    go (GetSourceInfo (Identity sourceIx) k) = do
      info <- lift $ C.sourceGet sourceIx
      go $ k (Identity info)

    go (GetSources spec k) = do
      -- we ignore the @X-Total-Count@ header for now
      sources <- getResponse <$> (lift $ C.sourcesGet spec)
      go $ k (Identity sources)

    go (Ingest name ingestOpts createOpts createIndices private input k) = do
      input'   <- traverse (liftIO . Client.streamFile) input
      progress <- C.sourcesPost ingestOpts
                                createOpts
                                (not createIndices)
                                private
                                (Just name)
                                input'
      info     <- showProgress progress
      go $ k info

    go (MakeTyped (Identity sourceIx) createIndices k) = do
      progress <- C.sourceMakeTyped sourceIx (not createIndices)
      info     <- showProgress progress
      go $ k info

    go (SetColumnType (Identity columnIx) typ k) = do
      NoContent <- lift $ C.columnSetType columnIx typ
      go $ k

    go (ManageDataset (Identity sourceNameIx) (Identity sourceIx) changes k) = do
      lift $ forM_ changes $ \change -> case change of
        SetDeprecated           x -> C.sourceSetDeprecated       sourceIx         x
        SetPublic               x -> C.sourceSetPublic           sourceNameIx     x
        SetGroupAccessLevel grp x -> C.sourceSetGroupAccessLevel sourceNameIx grp x
        SetUserAccessLevel  usr x -> C.sourceSetUserAccessLevel  sourceNameIx usr x
      go $ k

    go (ManageGroup group changes k) = do
      lift $ forM_ changes $ \change -> case change of
        AddUser           user -> C.groupAddUser     group user
        RemoveUser        user -> C.groupRemoveUser  group user
        GrantManageGroup  user -> C.groupAddAdmin    group user
        RevokeManageGroup user -> C.groupRemoveAdmin group user
      go $ k

    go (ManageUser changes k) = do
      lift $ forM_ changes $ \change -> case change of
        GrantCreateSource  user -> C.userSetCanCreateSource user True
        RevokeCreateSource user -> C.userSetCanCreateSource user False
        GrantCreateGroup   user -> C.userSetCanCreateGroup  user True
        RevokeCreateGroup  user -> C.userSetCanCreateGroup  user False
        CreateUser         user -> C.usersCreate            user
      go $ k

    go (CreateGroup name k) = do
      NoContent <- lift $ C.groupsCreate name
      go $ k

    go (TagSource (Identity sourceIx) tag k) = do
      NoContent <- lift $ C.sourceTagsPost sourceIx [tag]
      go $ k

    go (UntagSource (Identity sourceIx) tag k) = do
      NoContent <- lift $ C.sourceTagDelete sourceIx tag
      go $ k

    go (InferJsonType decompressMethod input k) = do
      input'   <- traverse (liftIO . Client.streamFile) input
      inferred <- lift $ C.sourceInferJsonType decompressMethod input'
      go $ k (Identity inferred)

    go (DownloadSource (Identity sourceIx) k) = do
      response <- C.sourceDownload sourceIx
      Client.responseBody response $$+- Conduit.stdout
      go $ k

    go (DeleteSources (Identity sourceIxs) k) = do
      forM_ sourceIxs $ \sourceIx -> do
        NoContent <- lift $ C.sourceDelete sourceIx
        pure ()
      go $ k

    go (Compact name createOptions createIndices private (Identity sourceIxs) k) = do
      progress <- C.sourcesCompact createOptions createIndices private sourceIxs (Just name)
      info <- showProgress progress
      go $ k info

showProgress :: (FromJSON a, MonadIO m)
             => Response (ResumableSource m BS.S.ByteString) -> m a
showProgress response = do
    logger <- liftIO $ newSimpleLogger stderr Notice
    result <- Client.responseBody response
         $$+- sinkProgress (liftIO . logMessage logger Notice)
    return result

sinkProgress :: forall m a. (MonadIO m, FromJSON a)
             => (SimpleProgress -> m ())
             -> Sink BS.S.ByteString m a
sinkProgress f =
    Conduit.linesUnboundedAscii =$= Conduit.mapM parse =$= go
  where
    go :: Sink (ProgressOr a) m a
    go = do
      mProgress <- await
      case mProgress of
        Nothing                   -> err "Missing final value"
        Just (ProgressComplete a) -> return a
        Just (ProgressOngoing  p) -> do
          case p of
            SimpleAborted Nothing  -> lift (f p) >> err "Unknown error"
            SimpleAborted (Just e) -> lift (f p) >> err e
            _otherwise             -> lift (f p) >> go

    parse :: BS.S.ByteString -> m (ProgressOr a)
    parse bs =
      case eitherDecode (BS.L.fromStrict bs) of
        Left  e  -> err $ unwords $
          [ "Failed to decode progress:", e
          , "in string" , BS.S.C8.unpack bs
          ]
        Right pa -> return pa

    err :: MonadIO n => String -> n x
    err msg = liftIO $ throwIO $ IngestAbortedException msg

data IngestAbortedException = IngestAbortedException String
  deriving (Show)

instance Exception IngestAbortedException

instance FriendlyException IngestAbortedException where
  displayFriendly (IngestAbortedException e) = "Ingest aborted: " ++ e
