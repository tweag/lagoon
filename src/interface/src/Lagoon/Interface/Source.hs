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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Lagoon.Interface.Source (
    SourceIx
  , Version(..)
  , SourceName
  , SourceNameIx(..)
  , Source(..)
  , Description
  , TagName
  ) where

import Data.Aeson
import GHC.Generics (Generic)
import Text.Show.Pretty
import Web.HttpApiData

import Lagoon.Interface.DB
import Lagoon.Interface.Pretty

-- | Source ID
--
-- TODO: Make this a newtype.
type SourceIx = Ix

-- | Source version
newtype Version = Version Int
  deriving (Show, PrettyVal, Pretty, FromJSON, ToJSON)

-- | Source name (identifying a particular dataset)
type SourceName = String

-- | Source description
type Description = String

-- |  Tags
type TagName = String

-- | Source name ID (in other words, ID of a version of a dataset)
newtype SourceNameIx = SourceNameIx Ix
  deriving (Show, PrettyVal, Pretty, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

-- | Source name along with its ID
data Source = Source {
      sourceNameIx :: SourceNameIx
    , sourceName   :: SourceName
    }
  deriving (Show, Generic)

instance Pretty Source where
  pretty = pretty . sourceName

instance PrettyVal Source

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

instance ToJSON Source where
  toJSON Source{..} = object [
      "name" .= sourceName
    , "ix"   .= sourceNameIx
    ]

instance FromJSON Source where
  parseJSON = withObject "Source" $ \obj -> do
    sourceName   <- obj .: "name"
    sourceNameIx <- obj .: "ix"
    return Source{..}

instance ToHttpApiData Version where
  toQueryParam (Version i) = toQueryParam i

instance FromHttpApiData Version where
  parseQueryParam = fmap Version . parseQueryParam
