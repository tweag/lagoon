module Pfizer.Datalake.Interface.Pretty (
    Pretty(..)
  , Doc
  , showHex
  ) where

import Data.Int
import Data.Text (Text)
import Data.Time
import Data.Word
import Text.PrettyPrint (Doc)
import qualified Data.ByteString           as BS.S
import qualified Data.ByteString.Lazy      as BS.L
import qualified Data.ByteString.UTF8      as BS.S.UTF8
import qualified Data.ByteString.Lazy.UTF8 as BS.L.UTF8
import qualified Data.Text                 as Text
import qualified Text.PrettyPrint          as PP
import Text.Printf

-- | Simple type class for outputting to the user
class Pretty a where
  pretty :: a -> Doc
  default pretty :: Show a => a -> Doc
  pretty = PP.text . show

  prettyStr :: a -> String
  prettyStr = PP.render . pretty

  prettyOneLine :: a -> String
  prettyOneLine = PP.renderStyle renderStyle . pretty
    where
      renderStyle = PP.style { PP.mode = PP.OneLineMode }

instance Pretty String where
  pretty = PP.text

instance Pretty Text where
  pretty = PP.text . Text.unpack

instance Pretty BS.S.ByteString where
  pretty = PP.text . BS.S.UTF8.toString

instance Pretty BS.L.ByteString where
  pretty = PP.text . BS.L.UTF8.toString

instance Pretty UTCTime
instance Pretty Bool
instance Pretty Int
instance Pretty Int32
instance Pretty Int64
instance Pretty Integer

showHex :: [Word8] -> Doc
showHex = PP.braces
        . PP.hsep
        . PP.punctuate PP.comma
        . map (PP.text . printf "%02X")
