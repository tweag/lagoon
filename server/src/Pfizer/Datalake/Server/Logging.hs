{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
module Pfizer.Datalake.Server.Logging (logging) where

import Data.ByteString.Builder (Builder)
import Data.CaseInsensitive (CI)
import Data.IORef
import Data.Monoid
import Data.Text (Text)
import Data.Vault.Lazy (Vault)
import GHC.Generics
import Network.Wai
import Network.Wai.Internal
import Network.Wai.Middleware.RequestLogger
import Network.Socket
import Text.Show.Pretty
import qualified Data.ByteString           as BS.S
import qualified Data.ByteString.Builder   as Builder
import qualified Data.ByteString.Lazy      as BS.L
import qualified Data.ByteString.Lazy.UTF8 as BS.L.UTF8
import qualified Data.ByteString.UTF8      as BS.S.UTF8
import qualified Data.CaseInsensitive      as CI
import qualified Data.Text                 as Text
import qualified Network.HTTP.Types        as H

import Pfizer.Datalake.Server.Cmdline

logging :: RequestLogging -> Middleware
logging RequestLoggingNone       = id
logging RequestLoggingProduction = logStdout
logging RequestLoggingDev        = logStdoutDev
logging RequestLoggingRaw        = logRawRequests

{-------------------------------------------------------------------------------
  Log raw requests
-------------------------------------------------------------------------------}

-- | Log the raw requests and responses
--
-- This is useful for debugging clients.
logRawRequests :: Middleware
logRawRequests app req k = do
    inMemReq <- toInMemRequest req
    putStrLn $ dumpStr inMemReq
    req' <- fromInMemRequest inMemReq
    app req' $ \resp -> do
      inMemResp <- toInMemResponse resp
      putStrLn $ dumpStr inMemResp
      let resp' = fromInMemResponse inMemResp
      k resp'

{-------------------------------------------------------------------------------
  In-memory versions of Request and Response

  In order to be able to pretty-print the full request and the full response,
  we need an in-memory representation. This obviously means any kind of
  streaming behaviour is lost; this is just for debugging.
-------------------------------------------------------------------------------}

data InMemResponse
    = InMemResponse     H.Status H.ResponseHeaders BS.L.ByteString
    | InMemResponseFile H.Status H.ResponseHeaders FilePath (Maybe FilePart)
  deriving (Generic, PrettyVal)

data InMemRequest = InMemRequest {
     inMemRequestMethod          :: H.Method
  ,  inMemHttpVersion            :: H.HttpVersion
  ,  inMemRawPathInfo            :: BS.S.ByteString
  ,  inMemRawQueryString         :: BS.S.ByteString
  ,  inMemRequestHeaders         :: H.RequestHeaders
  ,  inMemIsSecure               :: Bool
  ,  inMemRemoteHost             :: SockAddr
  ,  inMemPathInfo               :: [Text]
  ,  inMemQueryString            :: H.Query
  ,  inMemRequestBody            :: BS.L.ByteString
  ,  inMemVault                  :: Vault
  ,  inMemRequestBodyLength      :: RequestBodyLength
  ,  inMemRequestHeaderHost      :: Maybe BS.S.ByteString
  ,  inMemRequestHeaderRange     :: Maybe BS.S.ByteString
  ,  inMemRequestHeaderReferer   :: Maybe BS.S.ByteString
  ,  inMemRequestHeaderUserAgent :: Maybe BS.S.ByteString
  }
  deriving (Generic, PrettyVal)

toInMemResponse :: Response -> IO InMemResponse
toInMemResponse (ResponseRaw _ _) =
    error "ResponseRaw unsupported"
toInMemResponse (ResponseFile status headers fp part) =
    return $ InMemResponseFile status headers fp part
toInMemResponse (ResponseBuilder status headers builder) =
    return $ InMemResponse status headers (Builder.toLazyByteString builder)
toInMemResponse (ResponseStream status headers body) =
    InMemResponse status headers <$> fromStreamingBody body

fromInMemResponse :: InMemResponse -> Response
fromInMemResponse (InMemResponse status headers bs) =
    ResponseBuilder status headers (Builder.lazyByteString bs)
fromInMemResponse (InMemResponseFile status headers fp part) =
    ResponseFile status headers fp part

toInMemRequest :: Request -> IO InMemRequest
toInMemRequest Request{..} = do
    inMemRequestBody <- fromChunks requestBody
    return InMemRequest {
         inMemRequestMethod          = requestMethod
      ,  inMemHttpVersion            = httpVersion
      ,  inMemRawPathInfo            = rawPathInfo
      ,  inMemRawQueryString         = rawQueryString
      ,  inMemRequestHeaders         = requestHeaders
      ,  inMemIsSecure               = isSecure
      ,  inMemRemoteHost             = remoteHost
      ,  inMemPathInfo               = pathInfo
      ,  inMemQueryString            = queryString
      ,  inMemRequestBody            = inMemRequestBody
      ,  inMemVault                  = vault
      ,  inMemRequestBodyLength      = requestBodyLength
      ,  inMemRequestHeaderHost      = requestHeaderHost
      ,  inMemRequestHeaderRange     = requestHeaderRange
      ,  inMemRequestHeaderReferer   = requestHeaderReferer
      ,  inMemRequestHeaderUserAgent = requestHeaderUserAgent
      }

fromInMemRequest :: InMemRequest -> IO Request
fromInMemRequest InMemRequest{..} = do
    requestBody <- toChunks inMemRequestBody
    return Request {
         requestMethod          = inMemRequestMethod
      ,  httpVersion            = inMemHttpVersion
      ,  rawPathInfo            = inMemRawPathInfo
      ,  rawQueryString         = inMemRawQueryString
      ,  requestHeaders         = inMemRequestHeaders
      ,  isSecure               = inMemIsSecure
      ,  remoteHost             = inMemRemoteHost
      ,  pathInfo               = inMemPathInfo
      ,  queryString            = inMemQueryString
      ,  requestBody            = requestBody
      ,  vault                  = inMemVault
      ,  requestBodyLength      = inMemRequestBodyLength
      ,  requestHeaderHost      = inMemRequestHeaderHost
      ,  requestHeaderRange     = inMemRequestHeaderRange
      ,  requestHeaderReferer   = inMemRequestHeaderReferer
      ,  requestHeaderUserAgent = inMemRequestHeaderUserAgent
      }

{-------------------------------------------------------------------------------
  Auxiliary: from IO actions producing chunks to lazy bytestrings and back
-------------------------------------------------------------------------------}

fromChunks :: IO BS.S.ByteString -> IO BS.L.ByteString
fromChunks produceChunk = go []
  where
    go :: [BS.S.ByteString] -> IO BS.L.ByteString
    go cs = do
      c <- produceChunk
      if BS.S.null c
        then return $ BS.L.fromChunks (reverse cs)
        else go (c:cs)

toChunks :: BS.L.ByteString -> IO (IO BS.S.ByteString)
toChunks bs = do
    st <- newIORef $ BS.L.toChunks bs
    return $ go st
  where
    go :: IORef [BS.S.ByteString] -> IO BS.S.ByteString
    go st = do
      cs <- readIORef st
      case cs of
        []    -> return BS.S.empty
        c:cs' -> writeIORef st cs' >> return c

fromStreamingBody :: StreamingBody -> IO BS.L.ByteString
fromStreamingBody body = do
    st <- newIORef mempty
    body (append st) (return ())
    Builder.toLazyByteString <$> readIORef st
  where
    append :: IORef Builder -> Builder -> IO ()
    append st bldr = modifyIORef st (<> bldr)

{-------------------------------------------------------------------------------
  Some orphans (it's okay, this is just in the server executable)
-------------------------------------------------------------------------------}

instance PrettyVal a => PrettyVal (CI a) where
  prettyVal = prettyVal . CI.original

instance PrettyVal BS.S.ByteString where
  prettyVal = prettyVal . BS.S.UTF8.toString

instance PrettyVal BS.L.ByteString where
  prettyVal = prettyVal . BS.L.UTF8.toString

instance PrettyVal Text where
  prettyVal = prettyVal . Text.unpack

instance PrettyVal Vault where
  prettyVal _ = prettyVal "<<Vault>>"

deriving instance Generic FilePart
deriving instance Generic H.HttpVersion
deriving instance Generic H.Status
deriving instance Generic PortNumber
deriving instance Generic RequestBodyLength
deriving instance Generic SockAddr

deriving instance PrettyVal FilePart
deriving instance PrettyVal H.HttpVersion
deriving instance PrettyVal H.Status
deriving instance PrettyVal PortNumber
deriving instance PrettyVal RequestBodyLength
deriving instance PrettyVal SockAddr
