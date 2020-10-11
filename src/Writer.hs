module Writer (resolveWriterFunction) where

import qualified Data.ByteString as B
import           Data.Int        (Int64)
import           ElfWriter       (writeElf)
import           PhysMem         (Memory)

type WriterFunction = Int64 -> Memory -> B.ByteString

-- |Resolve an output format to a function that writes this format.
resolveWriterFunction :: String -> WriterFunction
resolveWriterFunction format = case format of
  "ELF" -> writeElf
  _     -> error "Unsupported output format. We support: ELF"

