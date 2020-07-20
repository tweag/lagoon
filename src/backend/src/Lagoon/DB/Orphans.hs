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
-- | Orphans for the datatypes declared in the @lagoon-interface@ library
--
-- The @lagoon-interface@ library is intended for use by both the frontend
-- and the backend, and as such must be compilable by @ghcjs@. This puts some
-- limitations on the dependencies it can have; in particular, it cannot depend
-- on @postgresql-simple@. We therefore declare a bunch of orphans here;
-- logically these class instances belong in the interface library.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lagoon.DB.Orphans () where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import qualified Data.ByteString.Char8 as BS.S.C8
import qualified Data.Vector           as V
import qualified Text.Parsec           as Parsec

import Lagoon.Interface
import Lagoon.Util.PostgreSQL

{-------------------------------------------------------------------------------
  Column types
-------------------------------------------------------------------------------}

-- | Textual representation
--
-- We use PostgreSQL native type names whenever possible
instance ToField ColumnType where
  toField ColBool            = toField "BOOLEAN"
  toField (ColInt I4)        = toField "INTEGER"
  toField (ColInt I8)        = toField "BIGINT"
  toField ColReal            = toField "DOUBLE PRECISION"
  toField ColText            = toField "TEXT"
  toField ColArr             = toField "int[]"
  toField ColDocument        = toField "DOCUMENT"
  toField (ColJSON Nothing)  = toField "JSON"
  toField (ColJSON (Just t)) = toField $ "JSON/" ++ prettyOneLine t
  toField (ColCustom (CustomType typ Nothing))  = toField $ typ
  toField (ColCustom (CustomType typ (Just f))) = toField $ typ ++ "/" ++ f
  toField (ColForeign (TableName table) (ColumnName col)) =
    toField $ "foreign on " ++ table ++ "/" ++ col

instance ToField Arr where
  toField (Arr xs) = toField (V.fromList xs)

instance FromField ColumnType where
  fromField fld bs = do
    str <- fromField fld bs
    case str of
      "BOOLEAN"               -> return $ ColBool
      "INTEGER"               -> return $ ColInt I4
      "BIGINT"                -> return $ ColInt I8
      "DOUBLE PRECISION"      -> return $ ColReal
      "TEXT"                  -> return $ ColText
      "DOCUMENT"              -> return $ ColDocument
      "JSON"                  -> return $ ColJSON Nothing
      ('J':'S':'O':'N':'/':t) -> case Parsec.parse parseJsonType "" t of
                                   Left  err -> fail $ show err
                                   Right typ -> return $ ColJSON (Just typ)
      (parseFK -> Right (table, col)) ->
                                 return $ ColForeign (TableName table) (ColumnName col)
      _  -> return $ ColCustom $
        case break (== '/') str of
          (typ, [])  -> CustomType typ Nothing
          (typ, _:f) -> CustomType typ (Just f)
    where parseFK = Parsec.parse
                    ((,) <$> ((Parsec.string "foreign on ")
                              *> Parsec.many Parsec.alphaNum <* Parsec.char '/')
                         <*> Parsec.many Parsec.alphaNum) ""

{-------------------------------------------------------------------------------
  Column
-------------------------------------------------------------------------------}

instance FromRow Column where
  fromRow = Column
      <$> field
      <*> field
      <*> field
      <*> field

{-------------------------------------------------------------------------------
  Permissions
-------------------------------------------------------------------------------}

instance FromField DatasetAccessLevel where
  fromField f mdata =
    case mdata of
      Nothing -> returnError UnexpectedNull f "DatasetAccessLevel"
      Just b  ->
        let str = BS.S.C8.unpack b in
        case str of
         'R':_ -> return DatasetAccessLevelRead
         'U':_ -> return DatasetAccessLevelUpdate
         'M':_ -> return DatasetAccessLevelManage
         _     -> returnError ConversionFailed f ("DatasetAccessLevel: " ++ str)

{-------------------------------------------------------------------------------
  Derived instances
-------------------------------------------------------------------------------}

deriving instance FromField ColumnIx
deriving instance FromField ColumnName
deriving instance FromField GroupIx
deriving instance FromField Ix
deriving instance FromField Schema
deriving instance FromField SourceNameIx
deriving instance FromField TableName
deriving instance FromField Timestamp
deriving instance FromField UserIx
deriving instance FromField Version
deriving instance FromField ViewName
deriving instance FromField SourcesCount

deriving instance ToField ColumnIx
deriving instance ToField ColumnName
deriving instance ToField GroupIx
deriving instance ToField Ix
deriving instance ToField Schema
deriving instance ToField SourceNameIx
deriving instance ToField TableName
deriving instance ToField Timestamp
deriving instance ToField UserIx
deriving instance ToField Version
deriving instance ToField ViewName
