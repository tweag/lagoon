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
module Lagoon.Ingest.Tabular.TypeInference (inferType) where

import Data.Conduit
import Data.Text (Text)
import qualified Data.Conduit.Combinators as C
import qualified Data.Text                as T

import Lagoon.Interface
import Lagoon.Ingest.Tabular.InferFieldType
import Lagoon.Ingest.Tabular.UntypedRecord
import Lagoon.Ingest.TypeUniverse
import Lagoon.Util.StrictList (StrictList(..))
import qualified Lagoon.Util.StrictList as StrictList

-- | Do type inference as we stream to the untyped table
inferType :: Monad m => Sink UntypedRecord m RowType
inferType = do
    mr <- await
    case mr of
      Nothing -> return $ RowType StrictList.Nil
      Just r  -> C.foldl updateGuess (initialGuess r)
  where
    initialGuess :: UntypedRecord -> RowType
    initialGuess = RowType . StrictList.fromList . map fromInferred . recordType

    updateGuess :: RowType -> UntypedRecord -> RowType
    updateGuess curHypothesis r = curHypothesis `updateWith` recordType r

-- | Infer type of a single record
recordType :: UntypedRecord -> [(Text, InferredType)]
recordType (UntypedRecord fs) = map (\txt -> (txt, inferFieldType txt)) fs

{-------------------------------------------------------------------------------
  Updating our type guess
-------------------------------------------------------------------------------}

-- | Update our guess
--
-- We use one (Haskell) type to represent our current guess of the (input) type,
-- and another (Haskell) type to represent the type of the next bit of the
-- input. The reason is that we want the former to be strict, as we don't want
-- to build up big thunks in our type guess; but we want the latter to be lazy,
-- because we might not need to know the (full) type of the next bit of the
-- input if (parts of) our guess have already reached the top of the lattice.
class UpdateGuess a where
  type UpdatedWith a :: *
  updateWith :: a -> UpdatedWith a -> a

instance UpdateGuess RowType where
  type UpdatedWith RowType = [(Text, InferredType)]

  updateWith :: RowType -> [(Text, InferredType)] -> RowType
  updateWith (RowType fs1) fs2 = RowType $ go fs1 fs2
    where
      go :: StrictList ColumnType -> [(Text, InferredType)] -> StrictList ColumnType
      go xs            []       = xs
      go Nil           ys       = StrictList.fromList (map fromInferred ys)
      go (Cons _ x xs) (y : ys) = StrictList.cons (updateWith x y) (go xs ys)

instance UpdateGuess ColumnType where
  type UpdatedWith ColumnType = (Text, InferredType)

  updateWith :: ColumnType -> (Text, InferredType) -> ColumnType
  -- Custom, foreign and JSON types are never inferred
  updateWith (ColForeign s s') _ = ColForeign s s'
  updateWith (ColJSON   typ) _   = ColJSON   typ
  updateWith (ColCustom typ) _   = ColCustom typ
  -- Once we inferred 'Text' or 'Document', we should avoid inferring any more
  -- types for this column as the result can only be 'Text' or 'Document'
  updateWith ColDocument _        = ColDocument
  updateWith ColText     (txt, _) = if T.length txt <= maxTextLen
                                      then ColText
                                      else ColDocument
  -- Similarly, if the inferred type is 'Text', the result must be 'Text'
  -- or 'Document', depending on the length of the field. The only exception
  -- if we already inferred 'Document', in which case the result must be
  -- 'Document' irrespective of the lenght of this field; but this case we
  -- already covered above.
  updateWith _ (txt, InfText) = if T.length txt <= maxTextLen
                                  then ColText
                                  else ColDocument
  -- If the two types are the same, nothing to do
  updateWith ColBool     (_, InfBool)   = ColBool
  updateWith ColReal     (_, InfReal)   = ColReal
  -- Match bitwidths
  updateWith (ColInt w1) (_, InfInt w2) = ColInt (w1 `updateWith` w2)
  -- Finally, specific cases for specific conversions
  updateWith ColBool     (_, InfInt w2) = ColInt w2
  updateWith ColBool     (_, InfReal)   = ColReal
  updateWith (ColInt w1) (_, InfBool)   = ColInt w1
  updateWith (ColInt _)  (_, InfReal)   = ColReal
  updateWith ColReal     (_, InfBool)   = ColReal
  updateWith ColReal     (_, InfInt _)  = ColReal
  -- TODO: check
  updateWith ColArr      _              = ColArr

instance UpdateGuess IntWidth where
  type UpdatedWith IntWidth = IntWidth

  updateWith :: IntWidth -> IntWidth -> IntWidth
  updateWith I8 _  = I8
  updateWith _  I8 = I8
  updateWith _  _  = I4
