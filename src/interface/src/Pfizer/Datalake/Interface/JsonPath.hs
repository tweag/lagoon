{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Pfizer.Datalake.Interface.JsonPath (
    JsonPath(..)
  , parseJsonPath
  ) where

import Data.Foldable (asum)
import Data.Monoid
import GHC.Generics (Generic)
import Text.Parsec (parse)
import Text.Parsec.Language (javaStyle)
import Text.Parsec.String (Parser)
import Text.Show.Pretty
import Web.HttpApiData
import qualified Data.Text         as Text
import qualified Text.Parsec.Token as P
import qualified Text.PrettyPrint  as PP

import Pfizer.Datalake.Interface.JsonType
import Pfizer.Datalake.Interface.Pretty

{-------------------------------------------------------------------------------
  Paths
-------------------------------------------------------------------------------}

data JsonPath =
    -- | Match anything
    P_

    -- | Match array
  | PA JsonPath

    -- | Match object
  | PO Key JsonPath
  deriving (Show, Generic, PrettyVal)

{-------------------------------------------------------------------------------
  Pretty-printer
-------------------------------------------------------------------------------}

-- | TODO: We should deal with escaping keys in the case for 'PO'
instance Pretty JsonPath where
  pretty P_       = "_"
  pretty (PA p)   = PP.brackets (pretty p)
  pretty (PO k p) = PP.braces (PP.doubleQuotes (pretty k) <> ":" <> pretty p)

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

instance ToHttpApiData JsonPath where
  toQueryParam = toQueryParam . prettyStr

instance FromHttpApiData JsonPath where
  parseQueryParam txt =
    case parseJsonPath (Text.unpack txt) of
      Left err -> Left  $ "Cannot parse '" <> txt <> "': " <> Text.pack err
      Right p  -> Right $ p

{-------------------------------------------------------------------------------
  Parser for Paths

  NOTE: This should be inverse to the pretty-printer.
-------------------------------------------------------------------------------}

parseJsonPath :: String -> Either String JsonPath
parseJsonPath = either (Left . show) Right . parse parserJsonPath ""

parserJsonPath :: Parser JsonPath
parserJsonPath = asum [
      P_ <$ name "_"
    , brackets $ PA <$> parserJsonPath
    , braces   $ PO <$> key <* op ":" <*> parserJsonPath
    ]
  where
    key :: Parser Key
    key = Key . Text.pack <$> str

lexer = P.makeTokenParser javaStyle {
              P.reservedNames   = ["_"]
            , P.reservedOpNames = [":"]
            }

brackets = P.brackets lexer
braces   = P.braces lexer
name     = P.reserved lexer
str      = P.stringLiteral lexer
op       = P.reservedOp lexer
