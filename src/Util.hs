module Util (sameByte)
  where

import qualified Data.ByteString.Lazy as BL
import           Data.Int             (Int64)
import           Data.Word            (Word8)

sameByte :: Int64 -> Word8 -> BL.ByteString
sameByte = BL.replicate
