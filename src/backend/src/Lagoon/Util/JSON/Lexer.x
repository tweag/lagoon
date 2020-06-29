{
-- | Lexical analysis for JSON objects
--
-- This lexical analyser is expressly designed to be used in streaming mode. We
-- expect to be fed chunks of the input, and we want to avoid requiring an
-- unbounded number of chunks before recognizing a token. Thus, we recognize
-- only tokens of a short length or tokens that can be arbitrary split (like
-- string contents without special characters). As a consequence, we need to be
-- quite liberal in how we define the tokens; for example, a precise definition
-- of numbers would specify that the number can contain at most one period, say;
-- but this becomes hard to track across chunks. Instead, we just allow any
-- number of periods.
--
-- For the specification, see
-- <http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf>
module Lagoon.Util.JSON.Lexer (Lexer(..), lexer) where

import Data.Int
import Data.Word
import qualified Data.ByteString.Lazy as BS.L

import Lagoon.Util.JSON.Token
}

$objStart = [\x7B]
$objEnd   = [\x7D]
$arrStart = [\x5B]
$arrEnd   = [\x5D]
$colon    = [\x3A]
$comma    = [\x2C]
$quote    = [\x22]
$escape   = [\x5C]
$slash    = [\x2F]
$hex      = [0-9A-Fa-f]
$white    = [\x09\x0A\x0D\x20]
$minus    = [\x2D]
$digit    = [0-9]
$dot      = [\x2E]
$exp      = [\x65\x45]
$sign     = [\x2B\x2D]
$strChar  = [^\x22\x5C] -- characters that can appear unescaped in strings
$numChar  = [$digit $dot $sign $exp] -- characters that make up numbers
$endOfNum = [\n [^$numChar]]

:-

-- structural tokens, literals, whitespace

<0>  $arrStart  { simple ArrayStart   0 }
<0>  $objStart  { simple ObjectStart  0 }
<0>  $arrEnd    { simple ArrayEnd     0 }
<0>  $objEnd    { simple ObjectEnd    0 }
<0>  $comma     { simple Comma        0 }
<0>  $colon     { simple Colon        0 }
<0>  "true"     { simple (Bool True)  0 }
<0>  "false"    { simple (Bool False) 0 }
<0>  "null"     { simple Null         0 }
<0>  $white+    { simple Whitespace   0 }

-- strings and numbers

<0>  $quote $strChar* $quote     { matched stringCompl 0      }
<0>  $quote $strChar*            { matched stringStart string }

<0>  ($minus | $digit) $numChar* $endOfNum { rematch NumberComplete 0      }
<0>  ($minus | $digit) $numChar*           { simple  NumberStart    number }

<string>  $strChar* $quote       { matched stringEnd      0      }
<string>  $strChar+              { matched stringContent  string }
<string>  $escape $quote         { simple  (escaped '"' ) string }
<string>  $escape $escape        { simple  (escaped '\\') string }
<string>  $escape $slash         { simple  (escaped '/' ) string }
<string>  $escape "b"            { simple  (escaped '\b') string }
<string>  $escape "f"            { simple  (escaped '\f') string }
<string>  $escape "n"            { simple  (escaped '\n') string }
<string>  $escape "r"            { simple  (escaped '\r') string }
<string>  $escape "t"            { simple  (escaped '\t') string }
<string>  $escape "u" $hex{4}    { matched hex4           string }

<number>  $numChar* $endOfNum    { rematch NumberEnd      0      }
<number>  $numChar+              { simple  NumberContent  number }

{

{-------------------------------------------------------------------------------
  Skeleton for Alex

  We provide no support for alexInputPrevChar because we do not make any use of
  right contexts (they are detrimental to performance).
-------------------------------------------------------------------------------}

type AlexInput = BS.L.ByteString -- ^ UTF-8 encoded byte sequence
type StartCode = Int

initInput :: AlexInput
initInput = BS.L.empty

enqueue :: AlexInput -> BS.L.ByteString -> AlexInput
enqueue = BS.L.append

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte bs =
    case BS.L.uncons bs of
      Nothing       -> Nothing
      Just (w, bs') -> Just (w, bs')

{-------------------------------------------------------------------------------
  Actions
-------------------------------------------------------------------------------}

data ActionResult = ActionResult {
      actionToken     :: Token
    , actionNextCode  :: StartCode
    , actionNextInput :: AlexInput
    }

-- | Type of the actions associated with tokens
type AlexAction =
     AlexInput    -- ^ Original input
  -> AlexInput    -- ^ Remaining input
  -> Int64        -- ^ Length of the match (in bytes, not characters)
  -> ActionResult

matched :: (BS.L.ByteString -> TokenClass) -> StartCode -> AlexAction
matched f sc inpOrig inpRem n = ActionResult{
      actionToken     = (f inpMatched, inpMatched)
    , actionNextCode  = sc
    , actionNextInput = inpRem
    }
  where
    inpMatched = BS.L.take n inpOrig

simple :: TokenClass -> StartCode -> AlexAction
simple t sc inpOrig inpRem n = ActionResult{
      actionToken     = (t, BS.L.take n inpOrig)
    , actionNextCode  = sc
    , actionNextInput = inpRem
    }

-- | Return a token, but leave the last char of the input to be matched again
rematch :: TokenClass -> StartCode -> AlexAction
rematch t sc inpOrig inpRem n = ActionResult{
      actionToken     = (t, inpMatched)
    , actionNextCode  = sc
    , actionNextInput = BS.L.cons (BS.L.head inpNotMatched) inpRem
    }
  where
    (inpMatched, inpNotMatched) = BS.L.splitAt (n - 1) inpOrig

{-------------------------------------------------------------------------------
  Lexer proper
-------------------------------------------------------------------------------}

data Lexer =
    -- | The scanner needs more data before it can recognize a token
    --
    -- Once the length of this data exceeds a certain threshold, we know that no
    -- matter how much data we will add, we will never successfully recognize a
    -- token and we can return a JSON error.
    NeedData AlexInput (BS.L.ByteString -> Lexer)

     -- | We have successfully recognized a token
  | Token Token Lexer

lexer :: Lexer
lexer = NeedData initInput (go 0 . enqueue initInput)
  where
    go :: StartCode -> AlexInput -> Lexer
    go sc inpOrig =
      case alexScan inpOrig sc of
        AlexEOF       -> NeedData inpOrig $ go sc . enqueue inpOrig
        AlexError _   -> NeedData inpOrig $ go sc . enqueue inpOrig
        AlexSkip  _ _ -> error "impossible (all rules have associated actions)"
        AlexToken inpRem _lenInChars mkToken ->
          let lenInBytes       = BS.L.length inpOrig - BS.L.length inpRem
              ActionResult{..} = mkToken inpOrig inpRem lenInBytes
          in Token actionToken $ go actionNextCode actionNextInput

    -- The matched string
    mkMatch :: AlexInput -> Int -> BS.L.ByteString
    mkMatch inp n = BS.L.take (fromIntegral n) inp

}
