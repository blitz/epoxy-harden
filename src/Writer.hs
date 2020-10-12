module Writer (resolveWriterFunction) where

import qualified Data.ByteString as B
import           Data.Int        (Int64)
import           PhysMem         (Memory)
import qualified Writer.Elf      as Elf

type WriterFunction = Int64 -> Memory -> B.ByteString

-- |Resolve an output format to a function that writes this format.
resolveWriterFunction :: String -> WriterFunction
resolveWriterFunction format = case format of
  "ELF" -> Elf.write
  _     -> error "Unsupported output format. We support: ELF"

