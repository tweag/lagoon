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
module Lagoon.Util.PostgreSQL.Keywords (
    avoidSpecialChars
  , avoidKeywords
  , MaxIdLen
  , getMaxIdLen
  ) where

import Control.Monad.IO.Class
import Data.Char (isLetter, isDigit)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Only(..))
import qualified Data.Text as Text

import Lagoon.Util.PostgreSQL.Transaction
import Lagoon.Util.PostgreSQL.Reserved

-- | Avoid special characters in names
--
-- We do not change the capitalization of the input, nor avoid keywords
-- (see 'avoidKeywords' if required). This means that the resulting name must
-- still be quoted.
--
-- NOTE: This function does not deal with maximum identifier length nor with
-- duplicate identifiers (obviously).
--
-- See <https://www.postgresql.org/docs/9.4/static/sql-syntax-lexical.html>
-- for the official rules for identifiers.
avoidSpecialChars :: String -> String
avoidSpecialChars = conform
  where
    -- Make sure the identifier conforms to the lexical rules
    --
    -- The manual says:
    --
    --   "Identifiers must begin with a letter (a-z, but also letters with
    --   diacritical marks and non-Latin letters) or an underscore (_).
    --   Subsequent characters in an identifier or key word can be letters,
    --   underscores, digits (0-9), or dollar signs ($). Note that dollar signs
    --   are not allowed in identifiers according to the letter of the SQL
    --   standard, so their use might render applications less portable."
    --
    -- NOTE: I'm assuming Data.Char.isLetter and PostgreSQL agree on what
    -- is a letter and what is not. A few tests with some unicode seem to
    -- confirm that this is indeed the case.
    conform :: String -> String
    conform []     = "_"
    conform (c:cs)
      | isLetter c = c   : map conformChar    cs
      | c == '_'   = c   : map conformChar    cs
      | otherwise  = '_' : map conformChar (c:cs)

    -- Helper to conform, called on all characters except the first
    -- We don't allow for dollar signs for maximum compatibility.
    conformChar :: Char -> Char
    conformChar c
      | isLetter c = c
      | c == '_'   = c
      | isDigit c  = c
      | otherwise  = '_'

-- | Prefix the name with an underscore if it is a reserved keyword.
--
-- The manual says:
--
--   "The SQL standard will not define a key word that contains digits or
--   starts or ends with an underscore, so identifiers of this form are
--   safe against possible conflict with future extensions of the standard"
--   <https://www.postgresql.org/docs/9.4/static/sql-keywords-appendix.html>
avoidKeywords :: String -> String
avoidKeywords cs
  | isReserved cs = '_' : cs
  | otherwise     = cs

-- | Maximum ID length (see 'getMaxIdLen')
type MaxIdLen = Int

getMaxIdLen :: MonadIO m => Transaction m MaxIdLen
getMaxIdLen = aux <$> query_ "SHOW max_identifier_length"
  where
    aux :: [Only Text] -> Int
    aux [Only t] = read (Text.unpack t)
    aux _        = error "getMaxIdLen: unexpected result"
