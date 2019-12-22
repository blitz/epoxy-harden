{-# LANGUAGE DeriveGeneric #-}
module MachineDescription where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Scientific      as Scientific
import           GHC.Generics

-- Data types for the machine description JSON file. These model the E820 map.

data MemoryMapType = Unknown | Available | Reserved | AcpiReclaimable | Nvs | BadRam
  deriving (Show, Enum, Eq, Bounded)

data MemoryMapEntry = MemoryMapEntry { baseAddr     :: Integer,
                                       memoryLength :: Integer,
                                       memoryType   :: MemoryMapType }
  deriving (Show, Generic)

type MemoryMap = [MemoryMapEntry]

newtype MachineDescription = MachineDescription { memoryMap :: MemoryMap }
  deriving (Show, Generic)

numberToMemoryMapType :: Int -> Maybe MemoryMapType
numberToMemoryMapType n = if checkBounds n then Just (toEnum n) else Nothing
  where checkBounds n = n >= fromEnum (minBound :: MemoryMapType) && n <= fromEnum (maxBound :: MemoryMapType)

instance FromJSON MemoryMapType where
  parseJSON (Number n) = case numberToMemoryMapType =<< Scientific.toBoundedInteger n of
    Just i  -> return i
    Nothing -> fail "Number is out of bounds for memory map types"
  parseJSON _ = fail "Expecting numeric value for memory map type"

instance FromJSON MemoryMapEntry where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier =
                                                replaceName "memory_type" "type"
                                                . replaceName "memory_length" "length"
                                                . camelTo2 '_' }
    where replaceName from to v
            | from == v = to
            | otherwise = v

instance FromJSON MachineDescription where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

-- Read the machine description

availableMemory :: MachineDescription -> Integer
availableMemory desc = sum [memoryLength m | m <- memoryMap desc, memoryType m == Available]

parseMachineDescription :: FilePath -> IO MachineDescription
parseMachineDescription file =
  decode <$> B.readFile file >>= maybe (fail "Failed to parse machine JSON file") return
