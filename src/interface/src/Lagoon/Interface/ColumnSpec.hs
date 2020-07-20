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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lagoon.Interface.ColumnSpec (
    ColumnIx(..)
  , ColumnSpec(..)
  , Column_(..)
  , Column
  , ColumnType(..)
  , ForeignSpec(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Web.HttpApiData
import qualified Text.PrettyPrint as PP

import Lagoon.Interface.ColumnType
import Lagoon.Interface.DB
import Lagoon.Interface.Pretty

{-------------------------------------------------------------------------------
  Column specification
-------------------------------------------------------------------------------}

newtype ColumnIx = ColumnIx Ix
  deriving (Show, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData)

-- | Information about the columns in a view
newtype ColumnSpec = ColumnSpec { columnSpecToList :: [Column] }
  deriving Eq

-- | TODO: Maybe move this out of here
data ForeignSpec = ForeignSpec {
      pointingColumn :: ColumnName
    , referencedTable  :: TableName
    , referencedColumn :: ColumnName }

-- | Information about a single column
--
-- The type argument @a@ is used for the column view name; we use this for
-- sanitization.
data Column_ a = Column {
      columnName   :: ColumnName
    , columnHeader :: Maybe Text
    , columnType   :: ColumnType
    , columnInView :: a
    }
  deriving (Functor, Eq)

type Column = Column_ ColumnName

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Pretty ColumnSpec where
  pretty (ColumnSpec spec) = PP.vcat $ "\tType\tName" : map aux spec
    where
      aux :: Column -> Doc
      aux Column{..} = PP.hcat $ PP.punctuate "\t" [
          pretty columnName
        , pretty columnType
        , case columnHeader of
            Nothing  -> "(no header)"
            Just hdr -> pretty hdr
          PP.<+> PP.parens (pretty columnInView)
        ]

{-------------------------------------------------------------------------------
  JSON
-------------------------------------------------------------------------------}

instance ToJSON a => ToJSON (Column_ a) where
  toJSON Column{..} = object [
      "name"   .= columnName
    , "header" .= columnHeader
    , "type"   .= columnType
    , "inView" .= columnInView
    ]

instance FromJSON a => FromJSON (Column_ a) where
  parseJSON (Object o) =
    Column <$> o .: "name"
           <*> o .: "header"
           <*> o .: "type"
           <*> o .: "inView"
  parseJSON _ = fail "(ColumnSpec.hs) Column_: no parse"

instance ToJSON ColumnSpec where
  toJSON (ColumnSpec cols) = toJSON cols

instance FromJSON ColumnSpec where
  parseJSON v = ColumnSpec <$> (parseJSON v)
