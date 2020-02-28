{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module MachineDescription where

import           Dhall

-- Data types for the machine description JSON file. These model the E820 map.

data MemoryMapType = Available | Device { key :: Text }
  deriving (Eq, FromDhall, Generic, Show)

data MemoryMapEntry = MemoryMapEntry { baseAddress  :: Natural,
                                       memoryLength :: Natural,
                                       memoryType   :: MemoryMapType }
  deriving (FromDhall, Generic, Show)

type MemoryMap = [MemoryMapEntry]

newtype MachineDescription = MachineDescription { memoryMap :: MemoryMap }
  deriving (FromDhall, Generic, Show)

-- Read the machine description

availableMemory :: MachineDescription -> Natural
availableMemory desc = sum [memoryLength m | m <- memoryMap desc, memoryType m == Available]

parseMachineDescription :: FilePath -> IO MachineDescription
parseMachineDescription = detailed . inputFile auto
