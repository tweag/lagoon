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
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lagoon.Client.Serialization () where

import Data.Proxy
import Servant.API
import Servant.Client
import Servant.Common.Req (setReqBody)
import Network.HTTP.Client (RequestBody)

import Lagoon.Interface
import Lagoon.Interface.API
import Lagoon.Client.Servant.QueryParamsEither ()
import Lagoon.Client.Servant.Conduit (genericMediaType)

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
