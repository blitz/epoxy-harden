{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MachineDescription where

import           Data.List (find)
import           Dhall     (FromDhall, Generic, Natural, Text, ToDhall, auto,
                            inputFile)

-- Data types for the machine description JSON file. These model the E820 map.

data MemoryMapType = Available
    | Device
    { key :: Text
    }
    deriving (Eq, FromDhall, Generic, Show, ToDhall)

data MemoryMapEntry = MemoryMapEntry
    { baseAddress  :: Natural
    , memoryLength :: Natural
    , memoryType   :: MemoryMapType
    }
    deriving (FromDhall, Generic, Show, ToDhall)

type MemoryMap = [MemoryMapEntry]

newtype MachineDescription = MachineDescription { memoryMap :: MemoryMap }
  deriving (FromDhall, Generic, Show, ToDhall)

availableMemory :: MachineDescription -> Natural
availableMemory desc = sum [memoryLength m | m <- memoryMap desc, memoryType m == Available]

-- Find a shared memory region that matches the given key.
findMemoryWithKey :: MachineDescription -> Text -> Maybe MemoryMapEntry
findMemoryWithKey desc keyToFind = find matchesKey $ memoryMap desc
  where matchesKey (MemoryMapEntry _ _ (Device k)) = keyToFind == k
        matchesKey _                               = False

-- | Parse the machine description from a file.
parseMachineDescription :: FilePath -> IO MachineDescription
parseMachineDescription = inputFile auto
