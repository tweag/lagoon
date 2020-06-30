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
module Pfizer.Datalake.Util.JSON.Stream (tokenize) where

import Data.Conduit
import Data.Int
import qualified Data.ByteString           as BS.S
import qualified Data.ByteString.Lazy      as BS.L
import qualified Data.ByteString.Lazy.UTF8 as BS.L.UTF8

import Pfizer.Datalake.Util.JSON.Errors
import Pfizer.Datalake.Util.JSON.Lexer
import Pfizer.Datalake.Util.JSON.Token

{-------------------------------------------------------------------------------
  Streaming interface to the scanner

  NOTE on performance:

  The performance of the scanner is improved when we match against longer
  tokens. For example, for strings, we try to match as much of a string as the
  current chunk will allow. Additionally, we also try to match the final quote,
  recognizing the entire string ('StringComplete') in one match; this alone
  improved performance of type inference by about 15%.

  To further increase the length of tokens, we try to match over several chunks
  at once. The exact number of chunks should be determined experimentally; I
  looked at the distribution of StringStart versus StringComplete in a JSON
  document with 100,000 objects (the association data from the Target Validation
  website):

  > Chunks | StringComplete | StringStart | Ratio
  > -------|----------------|-------------|------
  >      1 |        8355930 |        3213 | 0.038%
  >      2 |        8357493 |        1650 | 0.019%
  >      3 |        8357994 |        1149 | 0.013%
  >      4 |        8358248 |         895 | 0.010%

  Same data set, for numbers:

  > Chunks | NumberComplete | NumberStart | Ratio
  > ---------------------------------------------
  >      1 |        4399411 |         589 | 0.013%
  >      2 |        4399703 |         297 | 0.006%
  >      3 |        4399803 |         197 | 0.004%

  So for this data set I guess it's not all that relevant, probably because it
  doesn't contain many long strings or long numbers; for other data sets these
  numbers might be different.

  We might worry that the additional complexity in the scanner would lead to
  degraded performance, but this is not the case:

  > Chunks | StringComplete | NumberComplete | Time (s)
  > -----------------------------------------------
  >      1 |    Unsupported |    Unsupported | 9.271
  >      2 |    Unsupported |    Unsupported | 9.225
  >      3 |    Unsupported |    Unsupported | 9.174
  >      1 |      Supported |    Unsupported | 8.046
  >      2 |      Supported |    Unsupported | 7.980
  >      3 |      Supported |    Unsupported | 7.929
  >      1 |      Supported |      Supported | 7.084
  >      2 |      Supported |      Supported | 7.027
  >      3 |      Supported |      Supported | 6.679

  However, I also experimented with using a right-context for detecting the end
  of numbers (instead of using 'rematch'), but that /did/ lead to a rather
  significant performance degradation. So now we don't use any right contexts.

  I experimented with replacing the rules for true/false/null with a single rule
  for literals, but it actually decreased performance (albeit ever so slightly).

  There isn't much written about improving the performance of alex generated
  scanners; but some of the suggestions for Flex seem relevant;
  See <http://flex.sourceforge.net/manual/Performance.html>.
-------------------------------------------------------------------------------}

-- | Conduit wrapper around the lexical analyser
tokenize :: forall m. Monad m
         => Conduit BS.S.ByteString m (Either JsonError Token)
tokenize = go lexer
  where
    go :: Lexer -> Conduit BS.S.ByteString m (Either JsonError Token)
    go (NeedData inp k) =
      if (BS.L.length inp > threshold)
        then yield $ Left $ JsonError $
                  "Unrecognized: "
               ++ BS.L.UTF8.toString (BS.L.take threshold inp)
        else do
          bs <- getChunks
          if BS.L.null bs
            then return ()
            else go (k bs)
    go (Token t k) = do
      yield $ Right t
      go k

    getChunks :: Consumer BS.S.ByteString m BS.L.ByteString
    getChunks = aux [] numChunks
      where
        aux :: [BS.S.ByteString] -> Int -> Consumer BS.S.ByteString m BS.L.ByteString
        aux acc 0  = return $ BS.L.fromChunks (reverse acc)
        aux acc !n = do mBS <- await
                        case mBS of
                          Nothing -> aux acc      0
                          Just bs -> aux (bs:acc) (n-1)

{-------------------------------------------------------------------------------
  Parameters
-------------------------------------------------------------------------------}

-- | The threshold length of the input provided to the scanner so far such that
-- if we provided any more data, we would not increase our chances of matching
-- any token.
--
-- This is equal to the maximum of the minimum lengths of all tokens.
threshold :: Int64
threshold = 6

-- | Number of chunks to match on (see note on performance, above)
numChunks :: Int
numChunks = 3
