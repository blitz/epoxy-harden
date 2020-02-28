{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ApplicationDescription where

import           Data.Functor    ((<&>))
import           Data.List       (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Text       as T
import           Dhall

data GenericAddressSpaceDescElem elf = ELF
    { binary :: elf
    }
    | SharedMemory
    { key           :: Text
    , vaDestination :: Natural
    }
    deriving (Generic, Show)

instance FromDhall elf => FromDhall (GenericAddressSpaceDescElem elf)

type AddressSpaceDesc = [GenericAddressSpaceDescElem Text]

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

-- Map over all references in the application description. This will
-- be used to transform textual KObject references into global IDs.

kobjImplRefMap :: (a -> b) -> GenericKObjectImpl a e -> GenericKObjectImpl b e
kobjImplRefMap f Exit                                     = Exit
kobjImplRefMap f KLog{prefix=p}                           = KLog p
kobjImplRefMap f Process{pid=p, addressSpace=a, capabilities=c} = Process p a (f <$> c)
kobjImplRefMap f Thread{process=p}                        = Thread $ f p

kobjRefMap :: (a -> b) -> GenericKObject a e -> GenericKObject b e
kobjRefMap f KObject{gid=g, impl=i} = KObject (f g) (kobjImplRefMap f i)

asRefMap :: (a -> b) -> GenericApplicationDescription a e -> GenericApplicationDescription b e
asRefMap f ApplicationDescription{kobjects=k} = ApplicationDescription (kobjRefMap f <$> k)

-- Types as we get them from Dhall
type InputKObjectImpl = GenericKObjectImpl Text Text
type InputKObject = GenericKObject Text Text
type InputApplicationDescription = GenericApplicationDescription Text Text

-- Types as we use them later in this program
type KObjectImpl = GenericKObjectImpl Natural Text
type KObject = GenericKObject Natural Text
type ApplicationDescription = GenericApplicationDescription Natural Text

-- Conversion from input application config to the internal
-- representation. We replace textual IDs by sequential integers.

allGids :: InputApplicationDescription -> [Text]
allGids a = gid <$> kobjects a

type GidMap = Map.Map Text Natural

gidNameToNaturalMappings :: InputApplicationDescription -> GidMap
gidNameToNaturalMappings a = Map.fromList $ zip (allGids a) [0..]

normalizeAppDesc :: InputApplicationDescription -> ApplicationDescription
normalizeAppDesc a = asRefMap ((Map.!) gidMap) a
  where
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

processAddressSpace :: KObject -> [GenericAddressSpaceDescElem Text]
processAddressSpace KObject{impl=Process{addressSpace=a}} = a
processAddressSpace _                                     = error "not a process"

addressSpaceDescBinary :: [GenericAddressSpaceDescElem a] -> a
addressSpaceDescBinary (ELF{binary=b}:_) = b
addressSpaceDescBinary (_:rest)          = addressSpaceDescBinary rest
addressSpaceDescBinary []                = error "Address space has no ELF"

processBinary :: KObject -> String
processBinary = T.unpack . addressSpaceDescBinary . processAddressSpace

kobjectKind :: KObject -> Text
kobjectKind KObject{impl=Exit}      = "exit_kobject"
kobjectKind KObject{impl=KLog{}}    = "klog_kobject"
kobjectKind KObject{impl=Process{}} = "process"
kobjectKind KObject{impl=Thread{}}  = "thread"

parseApplicationDescription :: FilePath -> IO ApplicationDescription
parseApplicationDescription f = inputFile auto f <&> normalizeAppDesc
