{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Pfizer.Datalake.Util.PostgreSQL.CopyFrom (
    TupleLen
  , FieldLen
  , CopyTuple(..)
  , CopyField(..)
  , MoreRows(..)
  , copyFrom
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.ByteString.Builder (Builder)
import Data.Conduit
import Data.Int
import Data.List (foldl')
import Data.Monoid
import Data.Text (Text)
import qualified Data.ByteString            as BS.S
import qualified Data.ByteString.Builder    as Bld
import qualified Data.ByteString.Lazy       as BS.L
import qualified Data.Text.Encoding         as Text

#ifdef DEBUG_COPY
import Debug.Trace
import Data.Word
import Text.Printf
#endif

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.PostgreSQL.Transaction
import Pfizer.Datalake.Util.PostgreSQL.Quoted
import Pfizer.Datalake.Util.PostgreSQL.HList

{-------------------------------------------------------------------------------
  Support for COPY
-------------------------------------------------------------------------------}

type TupleLen = Int16
type FieldLen = Int32

class CopyTuple a where
  -- | Encoding for the whole tuple (with separate length prefix)
  copyTuple :: a -> (TupleLen, Builder)

class CopyField a where
  -- | Encoding for a single field (with separate length prefix)
  --
  -- NOTE: The length must be the length of the _encoded_ field (in bytes); for
  -- instance, if we are encoding text as UTF8, the length must be the length
  -- of the encoded UTF8 string.
  copyField :: a -> (FieldLen, Builder)

-- | Encoding for a single field, including the length prefix
copyField' :: CopyField a => a -> Builder
copyField' a = Bld.int32BE len <> bldr
  where
    (len, bldr) = copyField a

instance (CopyField a, CopyTuple b) => CopyTuple (a :- b) where
  copyTuple (a :- b) = (len + 1, copyField' a <> bldr)
    where
      (len, bldr) = copyTuple b

instance CopyField a => CopyTuple [a] where
  copyTuple as = (fromIntegral (length as), foldMap copyField' as)

instance (CopyField a, CopyField b) => CopyTuple (a, b) where
  copyTuple (a, b) = (2, copyField' a <> copyField' b)

instance (CopyField a, CopyField b, CopyField c) => CopyTuple (a, b, c) where
  copyTuple (a, b, c) = (3, copyField' a <> copyField' b <> copyField' c)

instance CopyField Text where
  copyField t = (fromIntegral (BS.S.length bs), Bld.byteString bs)
    where
      bs = Text.encodeUtf8 t

instance CopyField Int32 where
  copyField n = (4, Bld.int32BE n)

instance CopyField Arr where
  copyField (Arr xs) = squash alls
    where
      squash =
        foldl' (\(totLen, totBld) (len, bld) ->
        (totLen + len, totBld <> bld)) (0, mempty)
      -- TODO: explain
      alls =
        [ (4, Bld.int32BE 1)
        , (4, Bld.int32BE 0)
        , (4, Bld.int32BE 23)
        , (4, Bld.int32BE (fromIntegral $ length xs))
        , (4, Bld.int32BE 1)
        , (2 * 4 * fromIntegral (length xs), mconcat (copyField' <$> xs))
        ]

deriving instance CopyField Ix

-- | Are there any more rows to be processed?
data MoreRows =
    -- | Nope, all rows where ingested
    NoMoreRows

    -- | Yup, there are still more rows to be processed
    --
    -- This happens only if the number of columns in the source is larger than
    -- the expected number. We return the number of columns actually present.
  | StillMoreRows Int16

-- | High-level interface to the PostgreSQL COPY protocol
--
-- See <https://www.postgresql.org/docs/9.2/static/sql-copy.html> for the
-- low-level details.
--
-- Returns the number of rows ingested, and whether there are any rows still
-- pending. Any rows with fewer fields than expected will be padded with NULLs.
copyFrom :: forall m a. (MonadResource m, CopyTuple a)
       => (Schema, TableName)
       -> Int16        -- ^ Number of columns in the table
       -> Sink a (Transaction m) (Int64, MoreRows)
copyFrom qTable numColumns = do
    bracketT_ (copy_ ("COPY " <> quoted qTable <> " FROM STDIN BINARY"))
              (\moreRows -> (, moreRows) <$> putCopyEnd)
              (putCopyError "Copy aborted due to exception") $ do
      lift $ putBuilder $ header <> flags <> headerExtLen
      moreRows <- go
      lift $ putBuilder $ trailer
      return moreRows
  where
    go :: Sink a (Transaction m) MoreRows
    go = do
        ma <- await
        case ma of
          Nothing -> return NoMoreRows
          Just a  -> do
            let (len, bldr) = copyTuple a
            if | len == numColumns -> do
                   lift $ putBuilder $ mconcat [
                       Bld.int16BE numColumns
                     , bldr
                     ]
                   go
               | len < numColumns -> do
                   -- If we don't have enough fields, pad with NULL values
                   let padding :: Builder
                       padding = mconcat
                               $ replicate (fromIntegral (numColumns - len))
                                           (Bld.int32BE (-1)) -- NULL
                   lift $ putBuilder $ mconcat [
                       Bld.int16BE numColumns
                     , bldr
                     , padding
                     ]
                   go
               | otherwise -> do
                   leftover a
                   return $ StillMoreRows len

    header, flags, headerExtLen, trailer :: Builder
    header       = Bld.byteString "PGCOPY\n\xFF\r\n\0"
    flags        = Bld.int32BE 0
    headerExtLen = Bld.int32BE 0
    trailer      = Bld.int16BE (-1)

putBuilder :: MonadIO m => Builder -> Transaction m ()
putBuilder =
      putCopyLazy
#ifdef DEBUG_COPY
    . (\x -> traceShow (map (printf "%02X" :: Word8 -> String) $ BS.L.unpack x) x)
#endif
    . Bld.toLazyByteString

putCopyLazy :: MonadIO m => BS.L.ByteString -> Transaction m ()
putCopyLazy = mapM_ putCopyData . BS.L.toChunks
