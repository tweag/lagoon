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
-- | Add context to a JSON token stream
--
-- The JSON token stream is produced in constant space, but often this is not
-- quite enough information. In this module we add a context to the token
-- stream. Although the context can in theory grow linearly with the input,
-- this will only happen contains arbitrarily deeply nested arrays and objects.
-- In practice, adding a context effectively still gives constant space
-- behaviour.
module Lagoon.Util.JSON.Context (Context(..), addContext) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Conduit
import Data.Text (Text)
import qualified Data.Text as Text

import Lagoon.Interface
import Lagoon.Util.JSON.Errors
import Lagoon.Util.JSON.Token

{-------------------------------------------------------------------------------
  Context
-------------------------------------------------------------------------------}

data Context =
    -- | Top-level
    CT

    -- | Array
  | CA (Maybe Int) Context

    -- | Object
    --
    -- We record all keys we have previously seen so far in reverse order, as
    -- well as the key we're currently processing (if any)
  | CO (Maybe Key) [Key] Context
  deriving (Show)

{-------------------------------------------------------------------------------
  Matching contexts
-------------------------------------------------------------------------------}

-- | Matching context
--
-- Think of the context as a stack:
--
-- >   CT
-- >    |
-- >    |
-- >    v
-- >  -----
--
-- We split the context into two halves: the outer context, which matches
-- (a prefix of) the user specified path, and the inner context:
--
-- |   CT                                                P_
-- |    |                                                ^
-- |    |    outer context: matches (prefix of) path     |
-- |    v                                              -----
-- |  -----
-- |    |
-- |    |    inner context
-- |    v
-- |  -----
--
-- We distinguish between two cases:
--
-- 1. 'Matching': the context still matches the user's specified path; note that
--    if the path is 'PHere', /any/ context trivially matches.
--
--    INV1: For a 'Matching' context, the inner context must be 'CT' as long
--    as the path has not yet been fully matched (is not equal to 'PHere').
--
--    Note that the converse is not true: it is possible for the inner context
--    to be 'CT' even when the path has been fully matched; downstream
--    considers the inner context to be /the/ context and it can of course be
--    'CT'.
--
-- 2. 'Mismatched': the context has diverged from the user's path.
--
--    INV2: The inner context cannot be 'CT'.
--
--    Actually, INV2 follows from the definitions of the inner and outer
--    contexts: the outer context matches a prefix of the path, and we found
--    a mismatch; so the inner context here must have at least depth 1.

data MatchContext =
    Matching {
        _outer :: Context
      , _inner :: Context
      , _path  :: JsonPath
      }
  | Mismatch {
        _outer :: Context
      , _inner :: Context
      , _path  :: JsonPath
      }

-- | Push array context
pushArr :: Maybe Int -> MatchContext -> MatchContext
pushArr = \ix -> \case
    Matching o i p@(PA p') ->
      case ix of
        Just _        -> Matching (CA ix o) i         p'
        Nothing       -> Matching (CA ix o) i         p
    Matching o i p@P_ -> Matching o         (CA ix i) p
    Matching o i p    -> Mismatch o         (CA ix i) p
    Mismatch o i p    -> Mismatch o         (CA ix i) p

-- | Pop array
--
-- > Lemma: forall ix c. popArr (pushArr ix c) == Just (ix, c)
popArr :: MatchContext -> Maybe (Maybe Int, MatchContext)
popArr = \case
    Matching (CA ix o) i@CT p ->
      case ix of
        Just _                -> Just (ix, Matching o i (PA p))
        Nothing               -> Just (ix, Matching o i p)
    Matching o (CA ix i)    p -> Just (ix, Matching o i p)
    Mismatch o (CA ix i@CT) p -> Just (ix, Matching o i p)
    Mismatch o (CA ix i)    p -> Just (ix, Mismatch o i p)
    _otherwise                -> Nothing

-- | Push object context
pushObj :: Maybe Key -> [Key] -> MatchContext -> MatchContext
pushObj = \mk ks -> \case
    Matching o i p@(PO k' p') ->
      case mk of
        Just k
          | k == k'   -> Matching (CO mk ks o) i            p'
          | otherwise -> Mismatch o            (CO mk ks i) p
        Nothing       -> Matching (CO mk ks o) i            p
    Matching o i p@P_ -> Matching o            (CO mk ks i) p
    Matching o i p    -> Mismatch o            (CO mk ks i) p
    Mismatch o i p    -> Mismatch o            (CO mk ks i) p

-- | Pop object context
--
-- > Lemma: forall mk ks c. popObj (pushObj mk ks c) == Just (mk, ks, c)
popObj :: MatchContext -> Maybe (Maybe Key, [Key], MatchContext)
popObj = \case
    Matching (CO mk ks o) i@CT p ->
      case mk of
        Just k                   -> Just (mk, ks, Matching o i (PO k p))
        Nothing                  -> Just (mk, ks, Matching o i p)
    Matching o (CO mk ks i)    p -> Just (mk, ks, Matching o i p)
    Mismatch o (CO mk ks i@CT) p -> Just (mk, ks, Matching o i p)
    Mismatch o (CO mk ks i)    p -> Just (mk, ks, Mismatch o i p)
    _otherwise                   -> Nothing

{-------------------------------------------------------------------------------
  Proofs of the above lemmas
  ~~~~~~~~~~~~~~~~~~~~~~~~~~

  Lemma: forall mk ks c. popObj (pushObj mk ks c) == Just (mk, ks, c)

  Proof. By case analysis on c.

    Case (Matching o i p@(PO k' p')).

      NOTE: By INV1, i == CT.

      Case analysis on mk.

      Subcase (Just k), k == k'.

           popObj (Matching (CO mk ks o) i p')
        == Just (mk, ks, Matching o i (PO k p'))
        == Just (mk, ks, Matching o i (PO k' p'))

      Subcase (Just k), k /= k'.

           popObj (Mismatch o (CO mk ks i) p)
        == Just (mk, ks, Matching o i p)

      Subcase Nothing.

           popObj (Matching (CO mk ks o) i p)
        == Just (mk, ks, Matching o i p)

    Case (Matching o i p@P_).

           popObj (Matching o  (CO mk ks i) p)
        == Just (mk, ks, Matching o i p)

    Case (Matching o i p).

      NOTE: Since p /= P_, by INV1, i == CT.

          popObj (Mismatch o (CO mk ks i) p
       == Just (mk, ks, Matching o i p)

    Case (Mismatch o i p).

      NOTE: By INV2, i /= CT.

          popObj (Mismatch o (CO mk ks i) p
       == Just (mk, ks, Mismatch o i p)

    QED.

  The proof for arrays follows a similar structure, but is a bit simpler:

  Lemma: forall ix c. popArr (pushArr ix c) == Just (ix, c)

  Proof. Case analysis on c.

    Case (Matching o i p@(PA p')).

    NOTE: By INV1, i == CT.

    Case analysis on ix.

      Subcase (Just _).

           popArr (Matching (CA ix o) i p')
        == Just (ix, Matching o i (PA p'))

      Subcase Nothing.

           popArr (Matching (CA ix o) i p)
        == Just (ix, Matching o i p)

    Case (Matching o i p@P_).

          popArr (Matching o (CA ix i) p)
       == Just (ix, Matching o i p)

    Case (Matching o i p).

      NOTE: Since p /= P_, by INV1, i == CT.

         popArr (Mismatch o (CA ix i) p)
      == Just (ix, Matching o i p)

    Case (Mismatch o i p).

      NOTE: By INV2, i /= CT.

         popArr (Mismatch o (CA ix i) p)
      == Just (ix, Mismatch o i p)

    QED.
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Add context to the token stream
-------------------------------------------------------------------------------}

-- | Add context to a token stream
--
-- Invariant: every upstream token is passed downstream and in the same order.
--
-- The contexts change as follows. For arrays:
--
-- > [      c
-- >   ..   CArr (Just 1) c
-- > ,      CArr Nothing  c
-- >   ..   CArr (Just 2) c
-- > ]      CArr Nothing  c
--
-- For objects:
--
-- > {           c
-- >   "a":      CObj Nothing    []        c
-- >        ..   CObj (Just "a") []        c
-- > , "b":      CObj Nothing    ["a"]     c
-- >        ..   CObj (Just "b") ["a"      c
-- > }           CObj Nothing    ["a, "b"] c
--
-- This satisfies two constraints:
--
-- 1. We can "zoom in" to an array by restricting our attention to the tokens
--    in an @CArr (Just _)@ context; similarly, we can "zoom in" to element @k@
--    of an object by restricting our attention to the tokens in an
--    "CObj (Just k) _@ context.
-- 2. Type inference needs some information about the final context of an object
--    when it closes (in particular, it needs the keys of the object); this is
--    why we consider the closing brace to be part of @CObj Nothing ks@ context.
addContext :: forall m. MonadIO m => JsonPath -> Conduit Token m (Context, Token)
addContext = go . Matching CT CT
  where
    yieldIfMatched :: (MatchContext, Token) -> Producer m (Context, Token)
    yieldIfMatched (Matching _o i P_, t) = yield (i, t)
    yieldIfMatched _otherwise            = return ()

    go :: MatchContext -> Conduit Token m (Context, Token)
    go ctxt = withToken $ \t@(tc, _) ->
      case (tc, ctxt) of
        -- Scalars
        (StringComplete _ , c) -> yieldIfMatched (c, t) >> go c
        (StringStart    _ , c) -> yieldIfMatched (c, t) >> go c
        (StringContent  _ , c) -> yieldIfMatched (c, t) >> go c
        (StringEnd      _ , c) -> yieldIfMatched (c, t) >> go c
        (NumberComplete   , c) -> yieldIfMatched (c, t) >> go c
        (NumberStart      , c) -> yieldIfMatched (c, t) >> go c
        (NumberContent    , c) -> yieldIfMatched (c, t) >> go c
        (NumberEnd        , c) -> yieldIfMatched (c, t) >> go c
        (Bool _           , c) -> yieldIfMatched (c, t) >> go c
        (Null             , c) -> yieldIfMatched (c, t) >> go c
        (Whitespace       , c) -> yieldIfMatched (c, t) >> go c
        (Colon            , c) -> yieldIfMatched (c, t) >> go c

        (ArrayStart, c) -> do
          yieldIfMatched (c, t)
          go $ pushArr (Just 0) c
        (Comma, popArr -> Just (Just ix, c)) -> do
          yieldIfMatched (pushArr Nothing c, t)
          let !ix' = ix + 1
          go $ pushArr (Just ix') c
        (ArrayEnd, popArr -> Just (_, c)) -> do
          yieldIfMatched (pushArr Nothing c, t)
          go c

        (ObjectStart, c) -> do
          yieldIfMatched (c, t)
          withKey (pushObj Nothing [] c) $ \case
            Nothing -> go $ c
            Just k  -> go $ pushObj (Just k) [] c
        (Comma, popObj -> Just (Just k, ks, c)) -> do
          yieldIfMatched (pushObj Nothing (k:ks) c, t)
          withKey (pushObj Nothing (k:ks) c) $ \case
            Nothing -> unexpectedToken t
            Just k' -> go $ pushObj (Just k') (k:ks) c
        (ObjectEnd, popObj -> Just (Just k, ks, c)) -> do
          yieldIfMatched (pushObj Nothing (k:ks) c, t)
          go c

        -- Error cases
        _otherwise -> unexpectedToken t

    withKey :: MatchContext
            -> (Maybe Key -> Conduit Token m (Context, Token))
            -> Conduit Token m (Context, Token)
    withKey c k = withToken $ \t@(tc, _) -> do
        yieldIfMatched (c, t)
        case tc of
          StringComplete s -> withColon c $ k (Just (Key s))
          StringStart    s -> collect [s]
          Whitespace       -> withKey c k
          ObjectEnd        -> k Nothing
          _otherwise       -> unexpectedToken t
      where
        collect :: [Text] -> Conduit Token m (Context, Token)
        collect ss = withToken $ \t@(tc, _) -> do
            yieldIfMatched (c, t)
            case tc of
              StringContent s -> collect (s:ss)
              StringEnd     s -> withColon c $
                                   k $ Just (Key (Text.concat (reverse (s:ss))))
              _otherwise      -> error "Invalid JSON stream"

    withColon :: MatchContext
              -> Conduit Token m (Context, Token)
              -> Conduit Token m (Context, Token)
    withColon c k = withToken $ \t@(tc, _) -> do
        yieldIfMatched (c, t)
        case tc of
          Colon      -> k
          Whitespace -> withColon c k
          _otherwise -> unexpectedToken t

    withToken :: (Token -> Conduit Token m (Context, Token))
              -> Conduit Token m (Context, Token)
    withToken k = do
        mt <- await
        case mt of
          Nothing -> return ()
          Just t  -> k t

    unexpectedToken :: Token -> ConduitM a b m r
    unexpectedToken (tc, bs) = liftIO $
      throwIO $ JsonError $ "Unexpected token '" ++ prettyStr bs
                         ++ "' (" ++ show tc ++ ")"
