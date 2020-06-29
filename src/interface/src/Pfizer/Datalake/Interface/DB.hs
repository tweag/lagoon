{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Pfizer.Datalake.Interface.DB (
    -- * Column types
    Ix(..)
  , firstIx
  , Arr(..)
  , Timestamp(..)
    -- * DB schema
  , TableName(..)
  , ViewName(..)
  , TypeName(..)
  , ColumnName(..)
  , FunctionName(..)
  , TriggerName(..)
  , IndexName(..)
  , Schema(..)
    -- * Queries
  , SortDirection(..)
  , UserQuery(..)
    -- * COPY protocol
  , CopyToFormat(..)
  ) where

import Data.Aeson
import Data.Hashable (Hashable)
import Data.Int
import Data.String
import Data.Time
import GHC.Generics (Generic)
import Safe
import Text.Show.Pretty
import Web.HttpApiData
import qualified Data.Text        as Text
import qualified Text.PrettyPrint as PP

import Pfizer.Datalake.Interface.Pretty

{-------------------------------------------------------------------------------
  Column types
-------------------------------------------------------------------------------}

-- | Primary key we use to index tables
--
-- This has a 'ToIdent' instance because we use this to construct table names
-- and column names.
--
-- (We use @ix@ instead of @id@ to avoid confusion and name clashes with
-- Haskell's concept of identity.)
newtype Ix = Ix Int32
  deriving (Eq, Show, Enum, PrettyVal, FromJSON, ToJSON, Pretty)

newtype Arr = Arr [Int32]

-- | Key for the first row in a table
firstIx :: Ix
firstIx = Ix 1

-- | Timestamps
newtype Timestamp = Timestamp UTCTime
  deriving (Show, Eq, Pretty, FromJSON, ToJSON)

instance PrettyVal Timestamp where
  prettyVal (Timestamp ts) = prettyVal (show ts)

{-------------------------------------------------------------------------------
  DB schema
-------------------------------------------------------------------------------}

-- | Database schema to use (e.g. "public")
newtype Schema = Schema String
  deriving (Show, Eq, Hashable, PrettyVal, Pretty, IsString, FromJSON, ToJSON, Monoid)

-- | Database table name
newtype TableName = TableName String
  deriving (Show, Eq, Hashable, PrettyVal, Pretty, IsString, FromJSON, ToJSON, Monoid)

-- | Database view name
newtype ViewName = ViewName String
  deriving (Show, Eq, Hashable, PrettyVal, Pretty, IsString, FromJSON, ToJSON, Monoid)

-- | Database type name
newtype TypeName = TypeName String
  deriving (Show, Eq, Hashable, PrettyVal, Pretty, IsString, FromJSON, ToJSON, Monoid)

-- | Database column name
newtype ColumnName = ColumnName String
  deriving (Show, Eq, Hashable, PrettyVal, Pretty, IsString, FromJSON, ToJSON, Monoid)

-- | (SQL) function name
newtype FunctionName = FunctionName String
  deriving (Show, Eq, Hashable, PrettyVal, Pretty, IsString, FromJSON, ToJSON, Monoid)

-- | Trigger name
newtype TriggerName = TriggerName String
  deriving (Show, Eq, Hashable, PrettyVal, Pretty, IsString, FromJSON, ToJSON, Monoid)

-- | Index name
newtype IndexName = IndexName String
  deriving (Show, Eq, Hashable, PrettyVal, Pretty, IsString, FromJSON, ToJSON, Monoid)

{-------------------------------------------------------------------------------
  Pretty instances
-------------------------------------------------------------------------------}

instance Pretty (Schema, TableName) where
  pretty (Schema schema, TableName tableName) = PP.hcat $
    PP.punctuate "." [pretty schema, pretty tableName]

instance Pretty (Schema, ViewName) where
  pretty (Schema schema, ViewName viewName) = PP.hcat $
    PP.punctuate "." [pretty schema, pretty viewName]

instance Pretty (Schema, TableName, ColumnName) where
  pretty (Schema schema, TableName tableName, ColumnName columnName) = PP.hcat $
    PP.punctuate "." [pretty schema, pretty tableName, pretty columnName]

instance Pretty (Schema, ViewName, ColumnName) where
  pretty (Schema schema, ViewName viewName, ColumnName columnName) = PP.hcat $
    PP.punctuate "." [pretty schema, pretty viewName, pretty columnName]

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

data SortDirection = Ascending | Descending
  deriving (Show, Eq, Ord, Generic)

instance PrettyVal SortDirection

data UserQuery = UserQuery String
  deriving (Show)

instance FromJSON UserQuery where
  parseJSON = withObject "UserQuery" $ \obj ->
    UserQuery . fromString <$> obj .: "sql"

instance ToJSON UserQuery where
  toJSON (UserQuery q) = object [ "sql" .= q ]

{-------------------------------------------------------------------------------
  COPY protocol
-------------------------------------------------------------------------------}

-- | Format of a @COPY TO@ statement
--
-- Note that most of the functions in this module assume that the @COPY .. TO@
-- statement has already been issued.
data CopyToFormat =
    CopyToJSON
  | CopyToCSV
  deriving (Show)

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

instance FromHttpApiData Ix where
  parseQueryParam txt =
    case readMay (Text.unpack txt) of
      Nothing -> Left  $ "Invalid Ix"
      Just ix -> Right $ toEnum ix

instance ToHttpApiData Ix where
  toQueryParam (Ix ix) = toQueryParam ix

instance ToHttpApiData Timestamp where
  toQueryParam (Timestamp time) = toQueryParam $
                                    formatTime defaultTimeLocale "%F %T" time

instance FromHttpApiData Timestamp where
  parseQueryParam txt =
    case parseTimeM True defaultTimeLocale "%F %T" (Text.unpack txt) of
      Nothing -> Left  $ "Invalid Timestamp"
      Just t  -> Right $ Timestamp t

instance ToHttpApiData CopyToFormat where
  toQueryParam CopyToJSON = "application/json"
  toQueryParam CopyToCSV  = "text/csv"

instance FromHttpApiData CopyToFormat where
  parseQueryParam "application/json" = Right CopyToJSON
  parseQueryParam "text/csv"         = Right CopyToCSV
  parseQueryParam _                  = Left "Invalid CopyToFormat"

instance ToHttpApiData ColumnName where
  toQueryParam (ColumnName name) = toQueryParam name

instance FromHttpApiData ColumnName where
  parseQueryParam = Right . fromString . Text.unpack
