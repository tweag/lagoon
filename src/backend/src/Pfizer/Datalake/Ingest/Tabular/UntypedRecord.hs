{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pfizer.Datalake.Ingest.Tabular.UntypedRecord (
    UntypedRecord(..)
  , recordLength
  , readCsv
  ) where

import Control.Monad.Trans.Resource
import Database.PostgreSQL.Simple.ToRow
import Data.Conduit
import Data.CSV.Conduit
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.Conduit.List  as C
import qualified Data.ByteString    as BS.S
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text

import Pfizer.Datalake.Interface
import Pfizer.Datalake.Util.PostgreSQL

{-------------------------------------------------------------------------------
  Reading CSV files
-------------------------------------------------------------------------------}

-- | Raw records as they exist in the CSV file
newtype UntypedRecord = UntypedRecord [Text]
  deriving (Show, Generic, ToRow, CopyTuple, Eq)

-- | Number of fields in the record
recordLength :: UntypedRecord -> TupleLen
recordLength (UntypedRecord fs) = fromIntegral $ length fs

-- | Read all records from a CSV file
--
-- NOTE: The first row of the CSV file (headers) are returned just like any
-- other record. We do this because we (1) do our own type inference and hence
-- conversion anyway, and (2) want access to the header names. This information
-- is extracted by 'inferType'.
readCsv :: MonadThrow m
        => Delimiter
        -> EnableQuoteChar
        -> Encoding
        -> Conduit BS.S.ByteString m UntypedRecord
readCsv delim enableQuoteChar encoding =
    decode encoding =$= stripCR =$= intoCSV settings =$= C.map mkRecord
  where
    settings :: CSVSettings
    settings = CSVSettings {
          csvSep       = delimiterToChar delim
        , csvQuoteChar = if enableQuoteChar then Just '"' else Nothing
        }

    mkRecord :: Row Text -> UntypedRecord
    mkRecord = UntypedRecord

-- | Removes windows carriage return characters, which PostgreSQL chokes on.
stripCR :: MonadThrow m => Conduit Text m Text
stripCR = C.map stripWindowsLineEnding
  where
    stripWindowsLineEnding = Text.filter (/= '\r')

-- | Decode input stream
--
-- May throw a 'UnicodeException'.
decode :: MonadThrow m => Encoding -> Conduit BS.S.ByteString m Text
decode enc = awaitForever $ \bs ->
    case enc of
      Latin1 -> yield $ Text.decodeLatin1 bs
      UTF8   -> case Text.decodeUtf8' bs of
                  Left  ex  -> throwM ex
                  Right txt -> yield txt
