module ElfReader ( Elf, parseElfFile ) where

import qualified Data.ByteString as B
import           Data.Elf

parseElfFile :: FilePath -> IO Elf
parseElfFile path = parseElf <$> B.readFile path
