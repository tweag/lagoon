module Pfizer.Datalake.Interface.ColumnType (
    ColumnType(..)
  , IntWidth(..)
  , CustomType(..)
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import Text.Show.Pretty
import qualified Text.PrettyPrint as PP

import Pfizer.Datalake.Interface.DB
import Pfizer.Datalake.Interface.JsonType
import Pfizer.Datalake.Interface.Pretty

{-------------------------------------------------------------------------------
  Column types
-------------------------------------------------------------------------------}

-- | Column types for CSV files
data ColumnType =
    ColBool                  -- ^ Boolean
  | ColInt !IntWidth         -- ^ Integers
  | ColReal                  -- ^ Real numbers (floating point)
  | ColText                  -- ^ (Short) text fields (see 'maxTextLength')
  | ColArr                   -- ^ Array of ints
  | ColDocument              -- ^ (Long) text fields
  | ColJSON (Maybe JsonType) -- ^ JSON (type of the JSON value might not be known)
  | ColCustom CustomType     -- ^ Custom (user-defined) type
  | ColForeign TableName ColumnName -- ^ (Table, Column) TODO:
  deriving (Show, Generic, Eq)

-- | Bit-width of an integer field
data IntWidth = I4 | I8
  deriving (Show, Generic, Eq)

-- | Custom (user-specified) types
--
-- TODO: Although we allow for this in the types, we don't actually support
-- custom fields yet.
data CustomType = CustomType {
      customPostgresType :: String
    , customConversionFn :: Maybe String
    }
  deriving (Show, Generic, Eq)

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance PrettyVal ColumnType
instance PrettyVal IntWidth
instance PrettyVal CustomType

instance Pretty ColumnType where
  pretty ColBool            = pretty "BOOLEAN"
  pretty (ColInt I4)        = pretty "INTEGER"
  pretty (ColInt I8)        = pretty "BIGINT"
  pretty ColReal            = pretty "DOUBLE PRECISION"
  pretty ColText            = pretty "TEXT"
  pretty ColArr             = pretty "ARR"
  pretty ColDocument        = pretty "DOCUMENT"
  pretty (ColJSON Nothing)  = pretty "JSON"
  pretty (ColJSON (Just t)) = pretty "JSON" PP.<+> PP.parens (pretty t)
  pretty (ColCustom c)      = pretty "custom type" PP.<+> pretty c
  pretty (ColForeign s c)   = pretty "foreign on" PP.<+> pretty s PP.<+> PP.parens (pretty c)

instance Pretty CustomType where
  pretty (CustomType typ Nothing)  = pretty typ
  pretty (CustomType typ (Just f)) = formatted
    where
      formatted :: Doc
      formatted = PP.hsep [
          pretty typ
        , PP.parens $ pretty "with conversion function" PP.<+> pretty f
        ]

{-------------------------------------------------------------------------------
  JSON
-------------------------------------------------------------------------------}

foreignSourceLbl, foreignColumnLbl :: T.Text
foreignSourceLbl = T.pack "foreign_source"
foreignColumnLbl = T.pack "foreign_column"

-- | For consistency, we use PostgreSQL naming here, except where we additional
-- metadata
-- TODO: PG naming for ARR
instance ToJSON ColumnType where
  toJSON ColBool       = toJSON "BOOLEAN"
  toJSON (ColInt I4)   = toJSON "INTEGER"
  toJSON (ColInt I8)   = toJSON "BIGINT"
  toJSON ColReal       = toJSON "DOUBLE PRECISION"
  toJSON ColText       = toJSON "TEXT"
  toJSON ColArr        = toJSON "ARR"
  toJSON ColDocument   = toJSON "DOCUMENT"
  toJSON (ColJSON typ) = toJSON ("JSON", toJSON typ)
  toJSON (ColCustom CustomType{..}) = toJSON (customPostgresType, customConversionFn)
  toJSON (ColForeign sourceName (ColumnName colName)) =
    object [foreignSourceLbl .= sourceName
           ,foreignColumnLbl .= T.pack colName]

instance FromJSON ColumnType where
  parseJSON obj = (f =<< parseJSON obj) <|>
                  (g =<< parseJSON obj) <|>
                  (h =<< parseJSON obj) <|>
                  (i =<< parseJSON obj)
    where
      f "BOOLEAN" = pure ColBool
      f "INTEGER" = pure $ ColInt I4
      f "BIGINT" = pure $ ColInt I8
      f "DOUBLE PRECISION" = pure ColReal
      f "TEXT" = pure ColText
      f "ARR" = pure ColArr
      f "DOCUMENT" = pure ColDocument
      -- TODO: catch-all is dangerous
      f _ = fail "(ColumnType.hs) ColumnType: no parse"
      g ("JSON", x) = ColJSON <$> parseJSON x
      g _ = fail "(ColumnType.hs) ColumnType: no parse"
      h (pgTyp, convFn) = ColCustom <$>
        (CustomType <$> parseJSON pgTyp
                    <*> parseJSON convFn)
      i d = ColForeign <$> d .: foreignSourceLbl
                       <*> (ColumnName . T.unpack <$> d .: foreignColumnLbl)
