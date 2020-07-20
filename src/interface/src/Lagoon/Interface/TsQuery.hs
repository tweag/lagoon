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
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Lagoon.Interface.TsQuery (
    TsQuery(..)
  , TsLabel
  , tsQueryFromString
  ) where

import Control.Applicative (empty)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String
import Text.Parsec.Token (TokenParser)
import Text.PrettyPrint.HughesPJ ((<>), (<+>), text, maybeParens)
import Text.Show.Pretty
import qualified Text.Parsec.Token as P

import Lagoon.Interface.Pretty

{-------------------------------------------------------------------------------
  Fulltext queries
-------------------------------------------------------------------------------}

type TsLabel = String

-- | Full text query
data TsQuery =
    TsLexeme String -- ^ Should only contain letters
  | TsOr  TsQuery TsQuery
  | TsAnd TsQuery TsQuery
  | TsNot TsQuery
  | TsLabel TsLabel TsQuery
  deriving (Show)

instance PrettyVal TsQuery where
  prettyVal = prettyVal . prettyStr

tsQueryFromString :: String -> TsQuery
tsQueryFromString s =
    case parse parseTsQuery "query" s of
      Right q  -> fromMaybe (TsLexeme "") $ ignoreError q
      Left err -> error $ "tsQueryFromString: impossible: " ++ show err

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Pretty TsQuery where
  pretty = go 0
    where
      go :: Int -> TsQuery -> Doc
      go _ (TsLexeme l)  = pretty l
      go p (TsOr  q1 q2) = maybeParens (p > precOr) $
                             go precOr q1 <+> "|" <+> go precOr q2
      go p (TsAnd q1 q2) = maybeParens (p > precAnd) $
                             go precAnd q1 <+> go precAnd q2
      go p (TsNot q)     = maybeParens (p > precNot) $
                             "!" <> go precNot q
      go p (TsLabel l q) = maybeParens (p > precLabel) $
                             text l <> ":" <> go precLabel q

      precOr    = 1
      precAnd   = 2
      precNot   = 4
      precLabel = 5

{-------------------------------------------------------------------------------
  Query with embedded error terms
-------------------------------------------------------------------------------}

-- | Internal verrsion of 'TsQuery' that can contain error terms
--
-- This is crucial for being able to recover locally from errors.
data ParsedQuery =
    ParsedLexeme String -- ^ Should only contain letters
  | ParsedOr  ParsedQuery ParsedQuery
  | ParsedAnd ParsedQuery ParsedQuery
  | ParsedNot ParsedQuery
  | ParsedLabel TsLabel ParsedQuery
  | ParsedError

recover :: Parser ParsedQuery -> Parser ParsedQuery
recover p = p <|> pure ParsedError

ignoreError :: ParsedQuery -> Maybe TsQuery
ignoreError = go
  where
    go ParsedError       = Nothing
    go (ParsedLexeme l)  = Just $ TsLexeme l
    go (ParsedAnd q1 q2) = infixOp TsAnd q1 q2
    go (ParsedOr  q1 q2) = infixOp TsOr  q1 q2
    go (ParsedNot q)     = prefixOp TsNot q
    go (ParsedLabel l q) = prefixOp (TsLabel l) q

    infixOp :: (TsQuery -> TsQuery -> TsQuery)
            -> ParsedQuery -> ParsedQuery -> Maybe TsQuery
    infixOp f q1 q2 = case (go q1, go q2) of
                        (Nothing  , Nothing ) -> Nothing
                        (Just q1' , Nothing ) -> Just $ q1'
                        (Nothing  , Just q2') -> Just $ q2'
                        (Just q1' , Just q2') -> Just $ f q1' q2'

    prefixOp :: (TsQuery -> TsQuery) -> ParsedQuery -> Maybe TsQuery
    prefixOp f q = case go q of
                     Nothing -> Nothing
                     Just q' -> Just $ f q'

{-------------------------------------------------------------------------------
  Parser proper
-------------------------------------------------------------------------------}

parseTsQuery :: Parser ParsedQuery
parseTsQuery = whiteSpace *> parseExpr False <* eof

-- | Parser for queries
--
-- This parser is carefully written so that any string will have a successful
-- parse tree, albeit one possibly containing 'TsError' nodes.
--
-- The precedence levels of the operators is
--
-- > precedence | operator
-- > -----------+---------
-- >      1     | '|'
-- >      2     | '&'
-- >      3     | ' ' (treated as '&')
-- >      4     | '!'
-- >      5     | ':'
--
-- We don't use parsec's 'buildExpressionParser' because we want more control
-- over where we recover from errors.
parseExpr :: Bool -> Parser ParsedQuery
parseExpr inParens = parseP1
  where
    parseP1 :: Parser ParsedQuery
    parseP1 = foldl1 ParsedOr <$> recover parseP2 `sepBy1` (reservedOp "|")

    parseP2 :: Parser ParsedQuery
    parseP2 = foldl1 ParsedAnd <$> recover parseP3 `sepBy1` (reservedOp "&")

    parseP3 :: Parser ParsedQuery
    parseP3 = foldl1 ParsedAnd <$> many1 parseP4

    parseP4 :: Parser ParsedQuery
    parseP4 = asum [
        ParsedNot <$ reservedOp "!" <*> recover parseP5
      , parseP5
      ]

    parseP5 :: Parser ParsedQuery
    parseP5 = asum [
        try $ ParsedLabel <$> identifier <* reservedOp ":" <*> recover parseTerm
      , parseTerm
      ]

    parseTerm :: Parser ParsedQuery
    parseTerm = asum [
        between (symbol "(") (optional $ symbol ")") $ recover (parseExpr True)
      , ParsedLexeme <$> identifier
      , ParsedError  <$  unknown
      ]

    unknown :: Parser Char
    unknown = lexeme $ noneOf (if inParens then "&|!()" else "&|!(")

{-------------------------------------------------------------------------------
  Lexer
-------------------------------------------------------------------------------}

lexer :: TokenParser ()
lexer = P.makeTokenParser emptyDef {
      P.identStart      = alphaNum
    , P.identLetter     = alphaNum
    , P.reservedOpNames = ["!", "&", "|", ":"]
    , P.opStart         = oneOf "!&|:"
    , P.opLetter        = empty -- all operators are single characters
    }

identifier = P.identifier lexer
lexeme     = P.lexeme     lexer
reservedOp = P.reservedOp lexer
symbol     = P.symbol     lexer
whiteSpace = P.whiteSpace lexer
