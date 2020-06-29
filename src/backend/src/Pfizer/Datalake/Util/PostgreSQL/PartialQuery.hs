{-# LANGUAGE OverloadedStrings #-}
-- | Building up queries from parts
module Pfizer.Datalake.Util.PostgreSQL.PartialQuery (
    Clause(..)
  , PartialQuery(..)
  , partialQuery_
  , flattenPartialQuery
  , flattenCountQuery
  ) where

import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.ToRow
import Data.Bifunctor
import Data.List
import Data.Monoid
import Data.Ord (comparing)
import Data.String
import qualified Database.PostgreSQL.Simple as PG

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util

{-------------------------------------------------------------------------------
  Partial queries
-------------------------------------------------------------------------------}

-- | The various parts of an SQL expression
--
-- NOTE: The order in which these constructors are defined is important and must
-- match the order in which these clauses must appear in SQL.
data Clause =
    ClauseWhere
  | ClauseOrderBy
  | ClauseLimit
  | ClauseOffset
  deriving (Show, Eq, Ord)

-- | Part of an SQL query
--
-- See 'flattenPartialQuery'
data PartialQuery = forall a. (ToRow a, Show a)
                 => PartialQuery (Schema -> Query) a

instance Show PartialQuery where
  show (PartialQuery f a) =
       "PartialQuery_ "
    ++ "(\\schema -> " ++ show (f (Schema "schema")) ++ ") "
    ++ show a

-- | Constructor for 'PartialQuery' when the 'Schema' argument is not important.
partialQuery_ :: (ToRow a, Show a) => Query -> a -> PartialQuery
partialQuery_ q = PartialQuery (\_schema -> q)

instance Monoid PartialQuery where
  mempty = PartialQuery (\_schema -> mempty) ()
  PartialQuery q xs `mappend` PartialQuery q' xs' =
    PartialQuery (\schema -> q schema <> " " <> q' schema) (xs PG.:. xs')

instance IsString PartialQuery where
  fromString s = partialQuery_ (fromString s) ()

-- | Construct (part of) an SQL query from components
--
-- This makes it convenient to construct the various parts of an SQL query
-- without worrying about the right order etc., especially when this query
-- is the result of a computation of user input (for example, when the user is
-- presented with various ways to filter a set of results).
flattenPartialQuery :: [(Clause, PartialQuery)] -> PartialQuery
flattenPartialQuery =
      mconcat
    . map (uncurry flattenClause)
    . map (second (map snd))
    . groupOn fst
    . sortBy (comparing fst)
  where
    flattenClause :: Clause -> [PartialQuery] -> PartialQuery
    flattenClause ClauseWhere   qs = "WHERE "    <> intercalateM " AND " qs
    flattenClause ClauseOrderBy qs = "ORDER BY " <> intercalateM ", "    qs
    flattenClause ClauseLimit   qs = "LIMIT "    <> intercalateM " "     qs
    flattenClause ClauseOffset  qs = "OFFSET "   <> intercalateM " "     qs

-- | Construct (part of) an SQL query from components without taking @ORDER
-- BY@, @LIMIT@ and @OFFSET@ into account.
--
-- This is similar to 'flattenPartialQuery' but works in queries where column
-- names cannot be inspected (introduced by OrderBy) like simple aggregate
-- functions.
flattenCountQuery :: [(Clause, PartialQuery)] -> PartialQuery
flattenCountQuery =
      mconcat
    . map (uncurry flattenClause)
    . map (second (map snd))
    . groupOn fst
    . sortBy (comparing fst)
  where
    flattenClause :: Clause -> [PartialQuery] -> PartialQuery
    flattenClause ClauseWhere   qs  = "WHERE "    <> intercalateM " AND " qs
    flattenClause ClauseOrderBy _qs = ""
    flattenClause ClauseLimit   _qs = ""
    flattenClause ClauseOffset  _qs = ""
