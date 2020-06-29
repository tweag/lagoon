{-# LANGUAGE OverloadedStrings #-}
-- | Translate 'TsQuery' to PostgreSQL native syntax
module Pfizer.Datalake.Util.PostgreSQL.TsQuery (
    TsWeight(..)
  , tsQueryToPostgres
  ) where

import Database.PostgreSQL.Simple (Query, Only(..))
import Data.Monoid ((<>))
import Data.String

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.PostgreSQL.Transaction

{-------------------------------------------------------------------------------
  Translation to PostgreSQL syntax
-------------------------------------------------------------------------------}

-- | Weight of a search term
data TsWeight =
    TsWeightA
  | TsWeightB
  | TsWeightC
  | TsWeightD

-- | Translate our structured 'TsQuery' type to PostgreSQL syntax
--
-- See also 'tsQueryToPostgres'.
tsQueryToPostgres' :: [(TsLabel, TsWeight)] -> TsQuery -> String
tsQueryToPostgres' weights tsQuery =
    case tsQuery of
      TsLexeme "" -> "" -- special case for empty query
      _otherwise  -> go Nothing tsQuery
  where
    go :: Maybe TsLabel -> TsQuery -> String
    go lbl (TsLexeme l)  = l ++ ":*"
                             ++ maybe "" goWeight (lbl >>= (`lookup` weights))
    go lbl (TsOr  q1 q2) = infixOp  lbl "|" q1 q2
    go lbl (TsAnd q1 q2) = infixOp  lbl "&" q1 q2
    go lbl (TsNot q)     = prefixOp lbl "!" q
    go _   (TsLabel l q) = go (Just l) q

    goWeight :: TsWeight -> String
    goWeight TsWeightA = "A"
    goWeight TsWeightB = "B"
    goWeight TsWeightC = "C"
    goWeight TsWeightD = "D"

    infixOp  mLabel op q1 q2 = "(" ++ go mLabel q1 ++ op ++ go mLabel q2 ++ ")"
    prefixOp mLabel op q     = "(" ++ op ++ go mLabel q ++ ")"

-- | Translate 'TsQuery' to a 'Query' suitable for PostgreSQL
--
-- Returns 'Nothing' if the 'TsQuery' is empty (after removal of stop-words).
tsQueryToPostgres :: MonadIO m
                  => [(TsLabel, TsWeight)]
                  -> TsQuery
                  -> Transaction m (Maybe Query)
tsQueryToPostgres weights q = do
    [Only cnt] <- query_ $ "SELECT NUMNODE(TO_TSQUERY('english', '" <> q' <> "'))"
    return $ if cnt == emptyQuery then Nothing else Just q'
  where
    q' :: Query
    q' = fromString $ tsQueryToPostgres' weights q

    -- @numnode@ returns 0 for empty queries
    -- <https://www.postgresql.org/docs/9.4/static/textsearch-features.html#TEXTSEARCH-MANIPULATE-TSQUERY>
    emptyQuery :: Int
    emptyQuery = 0
