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

data AddressSpaceDescElem = ELF { binary :: Text }
                          | SharedMemory { key :: Text, vaDestination :: Natural }
  deriving (Show, FromDhall, Generic)

type AddressSpaceDesc = [AddressSpaceDescElem]

-- The appliation description is generic for different kernel object
-- reference types, because we get it first with textual references
-- and have to post-process it to use integers.
data GenericKObjectImpl ref = Exit
                            | KLog { prefix :: Text }
                            | Process { pid :: Natural, addressSpace :: AddressSpaceDesc,
                                        capabilities :: [ref]}
                            | Thread { process :: ref }
  deriving (Generic, Show)

data GenericKObject ref = KObject { gid :: ref, impl :: GenericKObjectImpl ref }
  deriving (Generic, Show)

data GenericApplicationDescription ref = ApplicationDescription { kobjects :: [GenericKObject ref]}
  deriving (Generic, Show)

instance FromDhall ref => FromDhall (GenericKObjectImpl ref)
instance FromDhall ref => FromDhall (GenericKObject ref)
instance FromDhall ref => FromDhall (GenericApplicationDescription ref)

instance Functor GenericKObjectImpl where
  fmap f Exit                                     = Exit
  fmap f KLog{prefix=p}                           = KLog p
  fmap f Process{pid=p, addressSpace=a, capabilities=c} = Process p a (f <$> c)
  fmap f Thread{process=p}                        = Thread $ f p

instance Functor GenericKObject where
  fmap f KObject{gid=g, impl=i} = KObject (f g) (f <$> i)

instance Functor GenericApplicationDescription where
  fmap f ApplicationDescription{kobjects=k} = ApplicationDescription (fmap f <$> k)

-- Types as we get them from Dhall
type InputKObjectImpl = GenericKObjectImpl Text
type InputKObject = GenericKObject Text
type InputApplicationDescription = GenericApplicationDescription Text

-- Types as we use them later in this program
type KObjectImpl = GenericKObjectImpl Natural
type KObject = GenericKObject Natural
type ApplicationDescription = GenericApplicationDescription Natural

-- Conversion from input application config to the internal
-- representation. We replace textual IDs by sequential integers.

allGids :: InputApplicationDescription -> [Text]
allGids a = gid <$> kobjects a

type GidMap = Map.Map Text Natural

gidNameToNaturalMappings :: InputApplicationDescription -> GidMap
gidNameToNaturalMappings a = Map.fromList $ zip (allGids a) [0..]

normalizeAppDesc :: InputApplicationDescription -> ApplicationDescription
normalizeAppDesc a = (Map.!) gidMap <$> a
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

processAddressSpace :: KObject -> AddressSpaceDesc
processAddressSpace KObject{impl=Process{addressSpace=a}} = a
processAddressSpace _                                     = error "not a process"

addressSpaceDescBinary :: AddressSpaceDesc -> Text
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
