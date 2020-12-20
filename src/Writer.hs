module Writer (resolveWriterFunction) where

import qualified Data.ByteString as B
import           Data.Int        (Int64)
import           Data.List       (find, intercalate)
import           PhysMem         (Memory)
import qualified Writer.Elf      as Elf

type WriterFunction = Int64 -> Memory -> B.ByteString

writerList :: [(String, WriterFunction)]
writerList = [
  ("riscv-elf64", Elf.write Elf.RiscV64),
  ("riscv-elf32", Elf.write Elf.RiscV32)
  ]

supportedFormats :: [String]
supportedFormats = fst <$> writerList

-- | Resolve an output format to a function that writes this format.
resolveWriterFunction :: String -> WriterFunction
resolveWriterFunction format =
  case find ((== format) . fst) writerList of
    Just (_, writerFn) -> writerFn
    Nothing -> error ("Unsupported output format. We support: " ++ (intercalate ", " supportedFormats))
