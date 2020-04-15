{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ApplicationDescription where

import           Data.List       (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Text       as T
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

-- The appliation description is generic for different kernel object
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

-- Map over all ELFs in the application description with
-- side-effects. This will be used to transform ELF file names to
-- actual loaded ELFs.
asElfMapM :: (a -> IO b) -> GenericApplicationDescription r a -> IO (GenericApplicationDescription r b)
asElfMapM f ApplicationDescription{kobjects=k} = do
  kobj <- mapM (kobjMap f) k
  return $ ApplicationDescription kobj
  where
    kobjMap :: (a -> IO b) -> GenericKObject r a -> IO (GenericKObject r b)
    kobjMap f KObject{gid=g, impl=i} = do
      mappedImpl <- implMap f i
      return $ KObject g mappedImpl

    implMap :: (a -> IO b) -> GenericKObjectImpl r a -> IO (GenericKObjectImpl r b)
    implMap f Exit = return Exit
    implMap f KLog{prefix=p} = return $ KLog p
    implMap f Process{pid=p, addressSpace=a, capabilities=c} = do
      mappedAs <- asMap f a
      return $ Process p mappedAs c
    implMap f Thread{process=p, stack=s} = return $ Thread p s

    asMap :: (a -> IO b) -> [GenericAddressSpaceDescElem a] -> IO [GenericAddressSpaceDescElem b]
    asMap = mapM . asElemMap

    asElemMap :: (a -> IO b) -> GenericAddressSpaceDescElem a -> IO (GenericAddressSpaceDescElem b)
    asElemMap f ELF{binary=b} = do
      mappedElf <- f b
      return $ ELF mappedElf
    asElemMap f SharedMemory{source=s, vaDestination=v, permissions=p} = return $ SharedMemory s v p

-- Map over all references in the application description. This will
-- be used to transform textual KObject references into global IDs.
asRefMap :: (a -> b) -> GenericApplicationDescription a e -> GenericApplicationDescription b e
asRefMap f ApplicationDescription{kobjects=k} = ApplicationDescription (kobjMap f <$> k)
  where
    kobjMap :: (a -> b) -> GenericKObject a e -> GenericKObject b e
    kobjMap f KObject{gid=g, impl=i} = KObject (f g) (implMap f i)

    implMap :: (a -> b) -> GenericKObjectImpl a e -> GenericKObjectImpl b e
    implMap f Exit = Exit
    implMap f KLog{prefix=p} = KLog p
    implMap f Process{pid=p, addressSpace=a, capabilities=c} = Process p a (f <$> c)
    implMap f Thread{process=p, stack=s} = Thread (f p) s

-- Types as we get them from Dhall
type InputKObjectImpl = GenericKObjectImpl Text Text
type InputKObject = GenericKObject Text Text
type InputApplicationDescription = GenericApplicationDescription Text Text

-- Types as we use them later in this program
type KObjectImpl = GenericKObjectImpl Natural Elf
type KObject = GenericKObject Natural Elf
type ApplicationDescription = GenericApplicationDescription Natural Elf

-- Conversion from input application config to the internal
-- representation. We replace textual IDs by sequential integers.

allGids :: InputApplicationDescription -> [Text]
allGids a = gid <$> kobjects a

type GidMap = Map.Map Text Natural

gidNameToNaturalMappings :: InputApplicationDescription -> GidMap
gidNameToNaturalMappings a = Map.fromList $ zip (allGids a) [0..]

normalizeAppDesc :: InputApplicationDescription -> IO ApplicationDescription
normalizeAppDesc a = asElfMapM (parseElfFile . T.unpack) globalGidAppDesc
  where
    -- TODO Print which ID didn't exist before we die, if Map.!
    -- failed.
    globalGidAppDesc = asRefMap ((Map.!) gidMap) a
    gidMap = gidNameToNaturalMappings a

isValidAppDesc :: InputApplicationDescription -> Bool
isValidAppDesc a = length gids == Set.size (Set.fromList gids)
  where gids = allGids a

-- Accessor functions

processes :: ApplicationDescription -> [KObject]
processes a = sortOn processPid $ filter isProcess (kobjects a)
  where isProcess KObject{impl=Process{}} = True
        isProcess _                       = False
        processPid KObject{impl=Process{pid=p}} = p
        processPid _                            = error "Not a process"

threads :: ApplicationDescription -> [KObject]
threads a = filter isThread (kobjects a)
  where isThread KObject{impl=Thread{}} = True
        isThread _                      = False

processPid :: KObject -> Natural
processPid KObject{impl=Process{pid=p}} = p
processPid _                            = error "not a process"

processCaps :: KObject -> [Natural]
processCaps KObject{impl=Process{capabilities=c}} = c
processCaps _                                     = error "not a process"

processAddressSpace :: KObject -> AddressSpaceDesc
processAddressSpace KObject{impl=Process{addressSpace=a}} = a
processAddressSpace _                                     = error "not a process"

addressSpaceDescBinary :: [GenericAddressSpaceDescElem a] -> a
addressSpaceDescBinary (ELF{binary=b}:_) = b
addressSpaceDescBinary (_:rest)          = addressSpaceDescBinary rest
addressSpaceDescBinary []                = error "Address space has no ELF"

processBinary :: KObject -> Elf
processBinary = addressSpaceDescBinary . processAddressSpace

parseApplicationDescription :: FilePath -> IO ApplicationDescription
parseApplicationDescription f = do
  inputAppDesc <- inputFile auto f
  normalizeAppDesc inputAppDesc
