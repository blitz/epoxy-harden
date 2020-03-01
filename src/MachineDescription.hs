{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module MachineDescription where

import           Data.List (find)
import           Dhall

-- Data types for the machine description JSON file. These model the E820 map.

data MemoryMapType = Available
    | Device
    { key :: Text
    }
    deriving (Eq, FromDhall, Generic, Show)

data MemoryMapEntry = MemoryMapEntry
    { baseAddress  :: Natural
    , memoryLength :: Natural
    , memoryType   :: MemoryMapType
    }
    deriving (FromDhall, Generic, Show)

type MemoryMap = [MemoryMapEntry]

newtype MachineDescription = MachineDescription { memoryMap :: MemoryMap }
  deriving (FromDhall, Generic, Show)

availableMemory :: MachineDescription -> Natural
availableMemory desc = sum [memoryLength m | m <- memoryMap desc, memoryType m == Available]

-- Find a shared memory region that matches the given key.
findMemoryWithKey :: MachineDescription -> Text -> Maybe MemoryMapEntry
findMemoryWithKey desc keyToFind = find matchesKey $ memoryMap desc
  where matchesKey (MemoryMapEntry _ _ (Device k)) = keyToFind == k
        matchesKey _                               = False

-- Read the machine description
parseMachineDescription :: FilePath -> IO MachineDescription
parseMachineDescription = inputFile auto
