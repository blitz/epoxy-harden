{-# LANGUAGE DeriveGeneric #-}

module ApplicationDescription where

import           Data.List (sortOn)
import           Dhall
import           ElfReader

data SharedMemorySource = NamedSharedMemory { sharedMemKey :: Text }
                        | AnonymousMemory { sharedMemSize :: Natural }
  deriving (Generic, Show)

instance FromDhall SharedMemorySource

data SharedMemoryPermissions = R | RW | RX
    deriving (Generic, Show)

instance FromDhall SharedMemoryPermissions

data GenericAddressSpaceDescElem elf = ELF
    { binary :: elf
    }
    | SharedMemory
    { source        :: SharedMemorySource
    , vaDestination :: Natural
    , permissions   :: SharedMemoryPermissions
    }
  deriving (Generic, Show)

instance FromDhall elf => FromDhall (GenericAddressSpaceDescElem elf)

type AddressSpaceDescElem = GenericAddressSpaceDescElem Elf
type AddressSpaceDesc = [AddressSpaceDescElem]

data ThreadStack = Auto | Fixed { vaInitStackPtr :: Natural }
  deriving (Generic, Show)

instance FromDhall ThreadStack

-- The application description is generic for different kernel object
-- reference types, because we get it first with textual references
-- and have to post-process it to use integers.
data GenericKObjectImpl ref elf = Exit
    | KLog
    { prefix :: Text
    }
    | Process
    { pid          :: Natural
    , addressSpace :: [GenericAddressSpaceDescElem elf]
    , capabilities :: [ref]
    }
    | Thread
    { process :: ref
    , stack   :: ThreadStack
    }
    deriving (Generic, Show)

data GenericKObject ref elf = KObject
    { gid  :: ref
    , impl :: GenericKObjectImpl ref elf
    }
    deriving (Generic, Show)

data GenericApplicationDescription ref elf = ApplicationDescription
    { kobjects :: [GenericKObject ref elf]
    }
    deriving (Generic, Show)

instance (FromDhall ref, FromDhall elf) => FromDhall (GenericKObjectImpl ref elf)
instance (FromDhall ref, FromDhall elf) => FromDhall (GenericKObject ref elf)
instance (FromDhall ref, FromDhall elf) => FromDhall (GenericApplicationDescription ref elf)

-- Types as we get them from Dhall
type InputKObjectImpl = GenericKObjectImpl Text Text
type InputKObject = GenericKObject Text Text
type InputApplicationDescription = GenericApplicationDescription Text Text

-- Types as we use them later in this program
type KObjectImpl = GenericKObjectImpl Natural Elf
type KObject = GenericKObject Natural Elf
type ApplicationDescription = GenericApplicationDescription Natural Elf

-- Accessor functions

isProcess :: GenericKObject r e -> Bool
isProcess KObject{impl=Process{}} = True
isProcess _                       = False

isThread :: GenericKObject r e -> Bool
isThread KObject{impl=Thread{}} = True
isThread _                      = False

processes :: GenericApplicationDescription r e -> [GenericKObject r e]
processes a = sortOn processPid $ filter isProcess (kobjects a)
  where processPid KObject{impl=Process{pid=p}} = p
        processPid _                            = error "Not a process"

threads :: GenericApplicationDescription r e -> [GenericKObject r e]
threads a = filter isThread (kobjects a)

processPid :: GenericKObject r e -> Natural
processPid KObject{impl=Process{pid=p}} = p
processPid _                            = error "not a process"

processCaps :: GenericKObject r e -> [r]
processCaps KObject{impl=Process{capabilities=c}} = c
processCaps _                                     = error "not a process"

processAddressSpace :: GenericKObject r e -> [GenericAddressSpaceDescElem e]
processAddressSpace KObject{impl=Process{addressSpace=a}} = a
processAddressSpace _                                     = error "not a process"

transformProcessAddressSpace :: GenericKObject r e -> ([GenericAddressSpaceDescElem e] -> [GenericAddressSpaceDescElem e]) -> GenericKObject r e
transformProcessAddressSpace KObject{gid = g, impl=Process{pid=p, addressSpace=a, capabilities=c}} f =
  KObject g (Process p (f a) c)
transformProcessAddressSpace _ _ = error "not a process"

addressSpaceDescBinary :: [GenericAddressSpaceDescElem a] -> a
addressSpaceDescBinary (ELF{binary=b}:_) = b
addressSpaceDescBinary (_:rest)          = addressSpaceDescBinary rest
addressSpaceDescBinary []                = error "Address space has no ELF"

processBinary :: KObject -> Elf
processBinary = addressSpaceDescBinary . processAddressSpace
