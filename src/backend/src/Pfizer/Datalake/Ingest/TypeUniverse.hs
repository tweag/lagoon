{-# LANGUAGE DeriveAnyClass #-}
-- | Type universe
module Pfizer.Datalake.Ingest.TypeUniverse (
    -- * Types
    InferredType(..)
  , RowType(..)
  , maxTextLen
  , fieldPostgresType
  , fromInferred
  ) where

import Data.String
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Show.Pretty
import qualified Data.Text as T

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.PostgreSQL
import Pfizer.Datalake.Util.StrictList

-- | The maximum length of a field to be considered 'Text'
--
-- If the field is longer than this, we will regard it as a 'Document' instead.
maxTextLen :: Int
maxTextLen = 4096

-- | Inferred types
--
-- We never infer custom or JSON fields.
data InferredType =
    InfBool
  | InfInt !IntWidth
  | InfReal
  | InfText
  deriving (Show, Generic, PrettyVal)

-- | A table type is given by the types of its fields
newtype RowType = RowType (StrictList ColumnType)
  deriving (Show, Generic, PrettyVal)

-- | Lift 'InferredType' into 'ColumnType'
fromInferred :: (Text, InferredType) -> ColumnType
fromInferred (t, InfText)
  | T.length t <= maxTextLen = ColText
  | otherwise                = ColDocument
fromInferred (_, InfBool)    = ColBool
fromInferred (_, InfInt w)   = ColInt w
fromInferred (_, InfReal)    = ColReal

{-------------------------------------------------------------------------------
  Conversion to PostgreSQL native types
-------------------------------------------------------------------------------}

fieldPostgresType :: ColumnType -> Query
fieldPostgresType = fromString . aux
  where
    aux :: ColumnType -> String
    aux ColBool     = "BOOLEAN"
    aux (ColInt I4) = "INTEGER"
    aux (ColInt I8) = "BIGINT"
    aux ColReal     = "DOUBLE PRECISION"
    aux ColText     = "TEXT"
    aux ColArr      = "int[]"
    aux ColDocument = "TEXT"
    aux (ColJSON _) = "JSONB"
    aux (ColCustom CustomType{..}) = customPostgresType
    aux (ColForeign _ _) = "INTEGER"
