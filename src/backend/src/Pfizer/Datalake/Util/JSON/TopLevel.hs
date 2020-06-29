{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Split JSON stream into top-level value
module Pfizer.Datalake.Util.JSON.TopLevel (FlatJSON(..), topLevel) where

import Data.Conduit
import qualified Data.ByteString.Builder as Bld
import qualified Data.ByteString.Lazy    as BS.L

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.JSON.Context
import Pfizer.Datalake.Util.JSON.Token
import Pfizer.Datalake.Util.PostgreSQL

{-------------------------------------------------------------------------------
  Flat JSON values
-------------------------------------------------------------------------------}

-- | Flat JSON
newtype FlatJSON = FlatJSON BS.L.ByteString
  deriving (Show, Pretty, ToField)

-- | JSONB support for PostgreSQL COPY
--
-- See <http://stackoverflow.com/questions/35600070/postgres-jsonb-specification-for-copy-in-binary-format>
instance CopyField FlatJSON where
  copyField (FlatJSON bs) = (
      fromIntegral (BS.L.length bs) + 1
    , Bld.word8 1 <> Bld.lazyByteString bs
    )

{-------------------------------------------------------------------------------
  Split into values
-------------------------------------------------------------------------------}

-- | Return all top-level values
--
-- NOTE: Memory usage of this conduit will be proportional to the length of
-- the longest value in the input stream. In particular, if the input stream
-- is a single JSON value, then this will read the entire input stream into
-- memory.
topLevel :: forall m. Monad m => Conduit (Context, Token) m FlatJSON
topLevel = go []
  where
    go :: [BS.L.ByteString] -> Conduit (Context, Token) m FlatJSON
    go acc = do
        mToken <- await
        case mToken of
          Nothing ->
            return ()
          Just (ctxt, (t, bs)) -> do
            let acc' = bs:acc
            if isComplete ctxt t
              then yield (FlatJSON $ BS.L.concat (reverse acc')) >> go []
              else go acc'

isComplete :: Context -> TokenClass -> Bool
isComplete c t =
    case t of
      ObjectStart      -> False
      ObjectEnd        -> popsToTop c
      ArrayStart       -> False
      ArrayEnd         -> popsToTop c
      StringComplete _ -> isTop c
      StringStart    _ -> False
      StringContent  _ -> False
      StringEnd      _ -> isTop c
      NumberComplete   -> isTop c
      NumberStart      -> False
      NumberContent    -> False
      NumberEnd        -> isTop c
      Bool _           -> isTop c
      Null             -> isTop c
      Comma            -> False
      Colon            -> False
      Whitespace       -> False

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

isTop :: Context -> Bool
isTop CT = True
isTop _  = False

popsToTop :: Context -> Bool
popsToTop CT         = True
popsToTop (CA _   c) = isTop c
popsToTop (CO _ _ c) = isTop c
