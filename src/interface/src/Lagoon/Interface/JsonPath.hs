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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Lagoon.Interface.JsonPath (
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

import Lagoon.Interface.JsonType
import Lagoon.Interface.Pretty

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
