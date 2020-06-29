{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pfizer.Datalake.Client.Serialization () where

import Data.Proxy
import Servant.API
import Servant.Client
import Servant.Common.Req (setReqBody)
import Network.HTTP.Client (RequestBody)

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Interface.API
import Pfizer.Datalake.Client.Servant.QueryParamsEither ()
import Pfizer.Datalake.Client.Servant.Conduit (genericMediaType)

{-------------------------------------------------------------------------------
  Composite query parameters
-------------------------------------------------------------------------------}

-- | TODO: I don't know how to split sourcesFilterFacts into 'fact's, 'class',
-- and 'group' so this is left empty for now.
instance HasClient sub => HasClient (SourcesSpec :> sub) where
  type Client (SourcesSpec :> sub) = SourcesSpec -> Client sub

  clientWithRoute Proxy req SourcesSpec{..} =
      clientWithRoute (Proxy :: Proxy (ExpandSourcesSpec sub)) req
        sourcesOffset
        sourcesLimit
        (prettyStr <$> sourcesSearchQuery)
        sourcesFilterIx
        sourcesFilterTags
        sourcesFilterDescription
        sourcesFilterName
        sourcesFilterUsers
        sourcesFilterColumns
        sourcesFilterCreatedAfter
        sourcesFilterCreatedBefore
        (map sortParam sourcesSortBy)
        sourcesIncludeDeprecated
    where
      sortParam :: (SourcesColumn, SortDirection)
                -> Either SourcesColumn SourcesColumn
      sortParam (col, Ascending)  = Left  col
      sortParam (col, Descending) = Right col

-- | Expand input
--
-- NOTE: Right now this relies on lazy bytestrings for streaming, because
-- that is what is hardcoded into the @servant-client@ 'Req' type.
instance HasClient sub => HasClient (Input :> sub) where
  type Client (Input :> sub) = Input RequestBody -> Client sub

  clientWithRoute Proxy req (Remote url) =
      clientWithRoute
        (Proxy :: Proxy (ExpandInput sub))
        req
        (Right url)
  clientWithRoute Proxy req (Upload path bs) =
      clientWithRoute
        (Proxy :: Proxy (ExpandInput sub))
        (setReqBody bs genericMediaType req)
        (Left path)

-- | Expand ingest options
instance HasClient sub => HasClient (IngestOptions :> sub) where
  type Client (IngestOptions :> sub) =
    IngestOptions -> Client sub

  clientWithRoute Proxy req IngestOptions{..} =
      clientWithRoute
        (Proxy :: Proxy (ExpandIngestOptions sub))
        req
        fileType
        (case hasHeaders of
           HasHeaders  -> Nothing
           NoHeaders i -> Just i)
        decompressMethod
        (Just jsonPath)
        (Just encoding)
        (fst <$> foreignIdentifier)
        (snd <$> foreignIdentifier)
        (case hasHeaders of
          HasHeaders  -> False
          NoHeaders _ -> True)
        (not enableQuoteChar)
        (not typeInference)
        (unSourceIdentifier <$> sourceIdentifier)

-- | Expand create options
instance HasClient sub => HasClient (CreateOptions :> sub) where
  type Client (CreateOptions :> sub) =
    CreateOptions -> Client sub

  clientWithRoute Proxy req CreateOptions{..} =
      clientWithRoute
        (Proxy :: Proxy (ExpandCreateOptions sub))
        req
        description
        (case createdTime of
           CreatedTimeNow     -> Nothing
           CreatedTimeFixed t -> Just t)
        tags
        (not createTypedTable)
        (Just logEvery)
