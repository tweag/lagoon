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
module Pfizer.Datalake.Util.JSON.Token (
    Token
  , TokenClass(..)
    -- ** Constructing tokens
  , stringCompl
  , stringStart
  , stringContent
  , stringEnd
  , escaped
  , hex4
  ) where

import Data.Bits (shift, (.|.))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Text            as Text

type Token = (TokenClass, BS.L.ByteString)

data TokenClass =
    ObjectStart
  | ObjectEnd
  | ArrayStart
  | ArrayEnd
  | StringComplete Text
  | StringStart Text
  | StringContent Text
  | StringEnd Text
  | NumberComplete
  | NumberStart
  | NumberContent
  | NumberEnd
  | Bool Bool
  | Null
  | Comma
  | Colon
  | Whitespace
  deriving (Show)

stringCompl :: BS.L.ByteString -> TokenClass
stringCompl = StringComplete . decodeUtf8 . BS.L.toStrict . BS.L.init . BS.L.tail

stringStart :: BS.L.ByteString -> TokenClass
stringStart = StringStart . decodeUtf8 . BS.L.toStrict . BS.L.tail

stringContent :: BS.L.ByteString -> TokenClass
stringContent = StringContent . decodeUtf8 . BS.L.toStrict

stringEnd :: BS.L.ByteString -> TokenClass
stringEnd = StringEnd . decodeUtf8 . BS.L.toStrict . BS.L.init

escaped :: Char -> TokenClass
escaped = StringContent . Text.singleton

-- | Convert unicode point in hex (\uABCD) to the corresponding character
--
-- TODO: is there a better way to define this conversion?
hex4 :: BS.L.ByteString -> TokenClass
hex4 bs =
    let a, b, c, d :: Word32
        [a, b, c, d] = map digitToInt . drop 2 . BS.L.unpack $ bs

        ch :: Word32
        ch = shift a 12 .|. shift b 8 .|. shift c 4 .|. d
    in StringContent $ Text.singleton (toEnum (fromIntegral ch))
  where
    digitToInt :: Word8 -> Word32

    digitToInt 0x30 = 0
    digitToInt 0x31 = 1
    digitToInt 0x32 = 2
    digitToInt 0x33 = 3
    digitToInt 0x34 = 4
    digitToInt 0x35 = 5
    digitToInt 0x36 = 6
    digitToInt 0x37 = 7
    digitToInt 0x38 = 8
    digitToInt 0x39 = 9

    digitToInt 0x41 = 10
    digitToInt 0x42 = 11
    digitToInt 0x43 = 12
    digitToInt 0x44 = 13
    digitToInt 0x45 = 14
    digitToInt 0x46 = 15

    digitToInt 0x61 = 10
    digitToInt 0x62 = 11
    digitToInt 0x63 = 12
    digitToInt 0x64 = 13
    digitToInt 0x65 = 14
    digitToInt 0x66 = 15

    digitToInt _   = 0 -- we don't want to throw errors
