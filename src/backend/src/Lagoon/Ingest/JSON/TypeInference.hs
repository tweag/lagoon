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
module Lagoon.Ingest.JSON.TypeInference (inferJsonType) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Conduit
import qualified Data.Map.Strict as Map

import Lagoon.Interface
import Lagoon.Util
import Lagoon.Util.JSON

{-------------------------------------------------------------------------------
  JSON type inference
-------------------------------------------------------------------------------}

inferJsonType :: forall m. MonadIO m => Sink (Context, Token) m JsonType
inferJsonType = go (Push JsonUnknown Empty)
  where
    go :: Stack JsonType -> Sink (Context, Token) m JsonType
    go Empty = error "impossible"
    go stack@(Push typ stack') = do
      mt <- await
      case mt of
        Nothing             -> return typ
        Just (ctxt, (t, _)) -> case (ctxt, t) of
          -- Arrays
          (_, ArrayStart) -> go $ Push JsonUnknown stack
          (_, ArrayEnd)   -> do
            let (typ', stack'') = pop stack'
                arrTyp          = JsonArray typ
            go $ Push (unify typ' arrTyp) stack''

          -- Objects
          (_               , ObjectStart) -> ignore -- we push when we see ":"
          (CO Nothing ks _ , ObjectEnd  ) -> do
            let (elems, stack'') = popZip ks stack
                elems'           = map (second (ElemType False)) elems
                (typ', stack''') = pop stack''
                objTyp           = JsonObject $ ObjectType (Map.fromList elems')
            go $ Push (unify typ' objTyp) stack'''
          (CO _  _  _      , Colon      ) -> go $ Push JsonUnknown stack
          (CO Nothing _ _  , _          ) -> ignore -- object keys

          -- Scalars
          (_, NumberComplete  ) -> go $ Push (unify typ JsonNumber) stack'
          (_, NumberEnd       ) -> go $ Push (unify typ JsonNumber) stack'
          (_, StringComplete _) -> go $ Push (unify typ JsonString) stack'
          (_, StringEnd      _) -> go $ Push (unify typ JsonString) stack'
          (_, Bool _          ) -> go $ Push (unify typ JsonBool)   stack'

          -- Null
          (_, Null) -> go $ Push (unify typ (JsonNullable JsonUnknown)) stack'

          -- Irrelevant tokens
          (_, NumberStart    ) -> ignore
          (_, NumberContent  ) -> ignore
          (_, StringStart   _) -> ignore
          (_, StringContent _) -> ignore
          (_, Whitespace     ) -> ignore
          (_, Comma          ) -> ignore

          -- Unexpected tokens
          (_, Colon)     -> unexpected
          (_, ObjectEnd) -> unexpected
      where
        ignore     = go stack
        unexpected = liftIO . throwIO . JsonError $ "Unexpected token"

{-------------------------------------------------------------------------------
  Stack
-------------------------------------------------------------------------------}

data Stack a = Empty | Push !a !(Stack a)
  deriving Show

pop :: Stack a -> (a, Stack a)
pop Empty       = error "pop: empty stack"
pop (Push a as) = (a, as)

popZip :: [b] -> Stack a -> ([(b, a)], Stack a)
popZip []     stack       = ([], stack)
popZip (_:_)  Empty       = error "popWith: empty stack"
popZip (b:bs) (Push a as) = let (bas, stack) = popZip bs as
                            in ((b, a):bas, stack)

{-------------------------------------------------------------------------------
  Unification
-------------------------------------------------------------------------------}

unify :: JsonType -> JsonType -> JsonType
unify = go
  where
    go :: JsonType -> JsonType -> JsonType
    go JsonMixed _ = JsonMixed
    go _ JsonMixed = JsonMixed

    go t JsonUnknown = t
    go JsonUnknown t = t

    go JsonString JsonString = JsonString
    go JsonNumber JsonNumber = JsonNumber
    go JsonBool   JsonBool   = JsonBool

    go (JsonNullable t) t' = JsonNullable (go t t')
    go t (JsonNullable t') = JsonNullable (go t t')

    go (JsonArray  t)  (JsonArray  t')  = JsonArray (go t t')
    go (JsonObject fs) (JsonObject fs') = JsonObject (aux fs fs')
      where
        aux :: ObjectType -> ObjectType -> ObjectType
        aux (ObjectType t1) (ObjectType t2) = ObjectType $
          adjustUnion goElem makeOptional makeOptional t1 t2

        makeOptional :: ElemType -> ElemType
        makeOptional (ElemType _ t) = ElemType True t

    go _ _ = JsonMixed

    goElem :: ElemType -> ElemType -> ElemType
    goElem (ElemType o1 t1) (ElemType o2 t2) = ElemType (o1 || o2) (go t1 t2)
