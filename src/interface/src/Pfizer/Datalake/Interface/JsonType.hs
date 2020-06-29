{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Pfizer.Datalake.Interface.JsonType (
    JsonType(..)
  , Key(..)
  , ObjectType(..)
  , ElemType(..)
  , parseJsonType
  ) where

import Control.Applicative ((<|>))
import Data.Aeson hiding (Value(..))-- (ToJSON(..), FromJSON(..), object, (.=), (.:))
import Data.Foldable (asum)
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Parsec.Language (javaStyle)
import Text.Parsec.String (Parser)
import Text.Show.Pretty
import qualified Data.Aeson.Types       as Aeson
import qualified Data.HashMap.Strict    as HashMap
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as Text
import qualified Text.Parsec.Combinator as P
import qualified Text.Parsec.Token      as P
import qualified Text.PrettyPrint       as PP

import Pfizer.Datalake.Interface.Pretty

{-------------------------------------------------------------------------------
  Type language
-------------------------------------------------------------------------------}

-- | Type of JSON values
data JsonType =
    JsonMixed
  | JsonUnknown
  | JsonString
  | JsonNumber
  | JsonBool
  | JsonNullable !JsonType
  | JsonArray !JsonType
  | JsonObject !ObjectType
  deriving (Show, Generic, Eq)

-- | Type of objects
newtype ObjectType = ObjectType { objectTypeToMap :: Map Key ElemType }
  deriving (Show, Eq)

-- | Object keys
newtype Key = Key Text
  deriving (Show, Eq, Ord, IsString)

-- | For object elements we record the type and whether it's optional
data ElemType = ElemType !Optional !JsonType
  deriving (Show, Generic, Eq)

type Optional = Bool

{-------------------------------------------------------------------------------
  PrettyVal (for debugging)
-------------------------------------------------------------------------------}

instance PrettyVal Key where
  prettyVal (Key k) = Con "Key" [String (Text.unpack k)]

instance PrettyVal ObjectType where
  prettyVal (ObjectType elems) = Con "fromList" [prettyVal (Map.toList elems)]

instance PrettyVal JsonType
instance PrettyVal ElemType

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Pretty Key where
  pretty (Key k) = pretty k

instance Pretty JsonType where
  pretty JsonMixed         = "mixed"
  pretty JsonUnknown       = "unknown"
  pretty JsonString        = "string"
  pretty JsonNumber        = "number"
  pretty JsonBool          = "bool"
  pretty (JsonNullable t)  = "nullable" PP.<+> pretty t
  pretty (JsonArray    t)  = PP.brackets $ pretty t
  pretty (JsonObject   fs) = PP.braces
                           . PP.sep
                           . PP.punctuate PP.comma
                           . map aux
                           . Map.toList
                           . objectTypeToMap
                           $ fs

    where
      -- TODO: I guess we should theoretically deal with escaping the
      -- keys here..
      aux :: (Key, ElemType) -> Doc
      aux (key, ElemType False typ) =
        PP.doubleQuotes (pretty key) <> ":" <> pretty typ
      aux (key, ElemType True  typ) =
        PP.doubleQuotes (pretty key) <> ":optional " <> pretty typ

{-------------------------------------------------------------------------------
  Parser

  NOTE: This should be inverse to the pretty-printer.
-------------------------------------------------------------------------------}

parseJsonType :: Parser JsonType
parseJsonType = asum [
      JsonMixed    <$  name "mixed"
    , JsonUnknown  <$  name "unknown"
    , JsonString   <$  name "string"
    , JsonNumber   <$  name "number"
    , JsonBool     <$  name "bool"
    , JsonNullable <$  name "nullable" <*> parseJsonType
    , JsonArray    <$> brackets parseJsonType
    , mkObject     <$> braces (parseElem `P.sepBy` comma)
    ]
  where
    mkObject :: [(Key, ElemType)] -> JsonType
    mkObject = JsonObject . ObjectType . Map.fromList

parseElem :: Parser (Key, ElemType)
parseElem = mkElem <$> str <* op ":"
                   <*> P.optionMaybe (name "optional")
                   <*> parseJsonType
  where
    mkElem :: String -> Maybe () -> JsonType -> (Key, ElemType)
    mkElem key Nothing   typ = (Key (Text.pack key), ElemType False typ)
    mkElem key (Just ()) typ = (Key (Text.pack key), ElemType True  typ)

lexer = P.makeTokenParser javaStyle {
              P.reservedOpNames = [":"]
            , P.reservedNames = [
                 "mixed"
               , "unknown"
               , "string"
               , "number"
               , "bool"
               , "nullable"
               , "optional"
               ]
            }

braces   = P.braces        lexer
brackets = P.brackets      lexer
comma    = P.comma         lexer
name     = P.reserved      lexer
op       = P.reservedOp    lexer
str      = P.stringLiteral lexer

{-------------------------------------------------------------------------------
  Serialization as a JSON value
-------------------------------------------------------------------------------}

instance ToJSON JsonType where
  toJSON JsonMixed          = "mixed"
  toJSON JsonUnknown        = "unknown"
  toJSON JsonString         = "string"
  toJSON JsonNumber         = "number"
  toJSON JsonBool           = "bool"
  toJSON (JsonNullable typ) = object ["nullable" .= toJSON typ]
  toJSON (JsonArray    typ) = object ["array"    .= toJSON typ]
  toJSON (JsonObject   typ) = object ["object"   .= toJSON typ]

instance ToJSON ObjectType where
  toJSON (ObjectType fields) = object $ map aux (Map.toList fields)
    where
      aux :: (Key, ElemType) -> Aeson.Pair
      aux (Key key, typ) = key .= typ

instance ToJSON ElemType where
  toJSON (ElemType False typ) = toJSON typ
  toJSON (ElemType True  typ) = object ["optional" .= toJSON typ]

instance FromJSON JsonType where
  parseJSON val | val == "mixed"   = pure JsonMixed
                | val == "unknown" = pure JsonUnknown
                | val == "string"  = pure JsonString
                | val == "number"  = pure JsonNumber
                | val == "bool"    = pure JsonBool
  parseJSON (Aeson.Object o) = JsonNullable <$> o .: "nullable" <|>
                               JsonArray    <$> o .: "array"    <|>
                               JsonObject   <$> o .: "object"
  parseJSON _ = fail "(JsonType.hs) JsonType: no parse"

instance FromJSON ObjectType where
  parseJSON = withObject "ObjectType" $ \o -> do
    let assocs = HashMap.toList o
        kvs    = mapM (\(k, y) -> (Key k,) <$> parseJSON y) assocs
    (ObjectType . Map.fromList) <$> kvs

instance FromJSON ElemType where
  parseJSON v =
    (case v of
      (Aeson.Object o) -> ElemType True <$> o .: "optional";
       _               -> fail "(JsonType.hs) ElemType: no parse") <|>
    (ElemType False <$> parseJSON v)
