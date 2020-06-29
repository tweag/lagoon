{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pfizer.Datalake.Server.Serialization () where

import Data.Maybe (fromMaybe)
import Network.URI
import Network.Wai.Conduit
import Servant
import Servant.Server.Internal.RoutingApplication

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Ingest (SourceBS)
import Pfizer.Datalake.Server.Servant.QueryParamsEither ()
import qualified Pfizer.Datalake.Interface.API as API

{-------------------------------------------------------------------------------
  Composite query parameters
-------------------------------------------------------------------------------}

instance HasServer sub ctxt => HasServer (SourcesSpec :> sub) ctxt where
  type ServerT (SourcesSpec :> sub) m = SourcesSpec -> ServerT sub m

  route Proxy ctxt sub =
      route (Proxy :: Proxy (API.ExpandSourcesSpec sub)) ctxt (adapt <$> sub)
    where
      adapt f
            sourcesOffset
            sourcesLimit
            rawQuery
            sourcesFilterIx
            sourcesFilterTags
            sourcesFilterDescription
            sourcesFilterName
            sourcesFilterUsers
            sourcesFilterColumns
            sourcesFilterCreatedAfter
            sourcesFilterCreatedBefore
            orders
            sourcesIncludeDeprecated
          =
        let sourcesReadableBy  = Nothing
            sourcesSearchQuery = tsQueryFromString <$> rawQuery
            sourcesSortBy      = case orders of
                                   [] -> [(SourcesCreated, Ascending)]
                                   _  -> map fromOrder orders
        in f SourcesSpec{..}

      fromOrder :: Either SourcesColumn SourcesColumn
                -> (SourcesColumn, SortDirection)
      fromOrder (Left  col) = (col, Ascending)
      fromOrder (Right col) = (col, Descending)

instance HasServer sub ctxt => HasServer (Input :> sub) ctxt where
  type ServerT (Input :> sub) m = Input SourceBS -> ServerT sub m

  route Proxy ctxt sub =
      route (Proxy :: Proxy (API.ExpandInput sub))
            ctxt
            (addBodyCheck (adapt <$> sub) (return ()) (\() -> bodyCheck))
    where
      adapt :: (Input SourceBS -> Server sub)
            -> SourceBS -> Maybe (Either FilePath URI) -> Server sub
      adapt f source = f . mkInput source

      mkInput :: u -> Maybe (Either FilePath URI) -> Input u
      mkInput u Nothing            = mkInput u (Just (Left ""))
      mkInput _ (Just (Right url)) = Remote url
      mkInput u (Just (Left fp))   = Upload fp u

      bodyCheck :: DelayedIO SourceBS
      bodyCheck = withRequest $ return . sourceRequestBody

instance HasServer sub ctxt => HasServer (IngestOptions :> sub) ctxt where
  type ServerT (IngestOptions :> sub) m =
    IngestOptions -> ServerT sub m

  route Proxy ctxt sub =
      route (Proxy :: Proxy (API.ExpandIngestOptions sub))
            ctxt
            (adapt <$> sub)
    where
      adapt f
            mFileType
            mPeekAt
            mDecompressMethod
            mJsonPath
            mEncoding
            mForeignMetadataSource
            mForeignMetadataField
            noHeaders
            disableQuoteChar
            noTypeInference
            mSourceIdentifier
        = f IngestOptions {
              fileType         = mFileType
            , hasHeaders       = if noHeaders
                                    then NoHeaders (fromMaybe 1000 mPeekAt)
                                    else HasHeaders
            , enableQuoteChar  = not disableQuoteChar
            , decompressMethod = mDecompressMethod
            , typeInference    = not noTypeInference
            , jsonPath         = fromMaybe P_ mJsonPath
            , encoding         = fromMaybe def mEncoding
            , foreignIdentifier= mkForeignIdentifier mForeignMetadataSource mForeignMetadataField
            , sourceIdentifier = mkSourceIdentifier <$> mSourceIdentifier
            }

      mkForeignIdentifier :: Maybe String -> Maybe String -> Maybe (String, String)
      mkForeignIdentifier s f = (,) <$> s <*> f

instance HasServer sub ctxt => HasServer (CreateOptions :> sub) ctxt where
  type ServerT (CreateOptions :> sub) m =
    CreateOptions -> ServerT sub m

  route Proxy ctxt sub =
      route (Proxy :: Proxy (API.ExpandCreateOptions sub))
            ctxt
            (adapt <$> sub)
    where
      adapt f
            mDescription
            mCreated
            tags
            noTyped
            mLogEvery
        = f CreateOptions {
              createTypedTable = not noTyped
            , description      = mDescription
            , tags             = tags
            , createdTime      = maybe CreatedTimeNow CreatedTimeFixed mCreated
            , logEvery         = fromMaybe maxBound mLogEvery
            }
