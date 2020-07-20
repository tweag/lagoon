{
module Lagoon.Ingest.Tabular.InferFieldType (inferFieldType) where

import Data.Text (Text)
import Data.Word
import qualified Data.Text.Array    as A
import qualified Data.Text.Internal as T

import Lagoon.Interface (IntWidth(..))
import Lagoon.Ingest.TypeUniverse
}

$digit  = 0-9
$eof    = [\x00]
$dot    = \x2E

@start  = $white*      -- left token context
@end    = $white* $eof -- right token context

@sign   = ("+" | "-")
@digits = $digit+

tokens :-

  -- Booleans
  -- <https://www.postgresql.org/docs/9.1/static/datatype-boolean.html>

  @start  [Tt]([Rr][Uu][Ee])?      @end { InfBool }
  @start  [Yy]([Ee][Ss])?          @end { InfBool }
  @start  [Oo][Nn]                 @end { InfBool }
  @start  "1"                      @end { InfBool }

  @start  [Ff]([Aa][Ll][Ss][Ee])?  @end { InfBool }
  @start  [Nn]([Oo])?              @end { InfBool }
  @start  [Oo][Ff][Ff]             @end { InfBool }
  @start  "0"                      @end { InfBool }

  -- Int
  -- <https://www.postgresql.org/docs/9.1/static/datatype-numeric.html#DATATYPE-INT>
  --
  -- We approximate the bit-width based on the string length; the PostgreSQL
  -- manual says smallint is usually only be used when disk space at a premium,
  -- so we ignore it, but also says that integer is faster than bigint and
  -- should be the default choice, so we do make that distinction.
  --
  -- It is possible that the input contains very long numbers though
  -- (gene sequences for instance); we recognize those as just text.
  --
  -- > 1234567890123456789
  -- > -------------------
  -- > 2147483648
  -- > 9223372036854775807

  @start  @sign? $digit{1,9}              @end { InfInt I4 }
  @start  @sign? $digit{1,9} $digit{1,9}  @end { InfInt I8 }

  -- Real numbers
  -- <https://www.postgresql.org/docs/9.1/static/datatype-numeric.html#DATATYPE-FLOAT>
  --
  -- Currently we just report everything as real numbers, and make no attempt to
  -- distinguish between single precision/double precision/arbitrary precision.
  --
  -- The syntax we support is also based on that of Postgres, which specifies:
  --
  -- > digitse[+-]digits
  -- > digits.[digits][e[+-]digits]
  -- > [digits].digits[e[+-]digits]
  --
  -- <https://www.postgresql.org/docs/9.4/static/sql-syntax-lexical.html>
  --
  -- TODO: PostgreSQL allows scientific notation even for integer values;
  -- we do not support this right now, and regard values in scientific notation
  -- as floating point values always.
  --
  -- We allow for a maximum of three digits for the exponent, as the range for
  -- the exponent for a double ranges from -308 to +308, so this is a crude
  -- way to detect values that would throw an SQL "out of range for type
  -- double precision" error.
  -- <https://en.wikipedia.org/wiki/Double-precision_floating-point_format>

  @start  @sign? @digits                 "e" @sign? $digit{1,3}    @end { InfReal }
  @start  @sign? @digits  $dot @digits? ("e" @sign? $digit{1,3})?  @end { InfReal }
  @start  @sign? @digits? $dot @digits  ("e" @sign? $digit{1,3})?  @end { InfReal }

{
-- | Input to the lexer (internal type)
--
-- We (ab/re)use the 'Text' data type to keep track of where we are in the
-- input; a length of 0 indicating we've processed every character but have
-- not yet returned end-of-input, and a negative length indicating we're done.
type AlexInput = Text

endOfInput, nonASCII :: Word8
endOfInput = 0x00
nonASCII   = 0x7F

-- | Get the next byte from the input
--
-- We could reduce the number of comparisons we need to make here by one
-- if we created a new array ahead of time which contains the end-of-input
-- symbol. However, this will be more efficient if we actually /reach/ the
-- end of the input; in most cases, the lexer should be able to conclude
-- well before the end of the input that none of the rules match.
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (T.Text _   _   (-1)) = Nothing
alexGetByte (T.Text arr off 0   ) = Just ( endOfInput, T.Text arr off (-1) )
alexGetByte (T.Text arr off len ) = Just ( discardUTF8 (A.unsafeIndex arr off)
                                         , T.Text arr (off + 1) (len - 1)
                                         )

-- | Throw away any non-ASCII characters
--
-- Alex works on UTF8 strings, but we're working with Text. We could first
-- translate back to UTF8 encoding, but that would be slow. Instead, we map all
-- characters outside the ASCII character set to 'nonASCII', so that none of our
-- rules will match, except for the default (Text).
discardUTF8 :: Word16 -> Word8
discardUTF8 n = if n < 128 then fromIntegral n else nonASCII

inferFieldType :: Text -> InferredType
inferFieldType txt =
    case alexScan txt 0 of
      AlexEOF                -> InfText
      AlexError _txt'        -> InfText
      AlexSkip  _txt' _n     -> InfText
      AlexToken _txt' _n typ -> typ
}
