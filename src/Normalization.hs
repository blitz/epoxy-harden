module Normalization (normalizeApplicationDescription) where

import           ApplicationDescription
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)
import qualified Data.Text              as T
import           ElfReader
import           Numeric.Natural
import           System.FilePath.Posix  (joinPath)

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

-- Conversion from input application config to the internal
-- representation. We replace textual IDs by sequential integers.

allGids :: InputApplicationDescription -> [Text]
allGids a = gid <$> kobjects a

type GidMap = Map.Map Text Natural

gidNameToNaturalMappings :: InputApplicationDescription -> GidMap
gidNameToNaturalMappings a = Map.fromList $ zip (allGids a) [0..]

-- Normalize an application description by turning all textual
-- references into numerical IDs and load all external resourceses
-- (ELF binaries).
normalizeApplicationDescription :: FilePath -> InputApplicationDescription -> IO ApplicationDescription
normalizeApplicationDescription root a = asElfMapM (parseElfFile . (\f -> joinPath [root, f]) . T.unpack) globalGidAppDesc
  where
    -- TODO Print which ID didn't exist before we die, if Map.!
    -- failed.
    globalGidAppDesc = asRefMap ((Map.!) gidMap) a
    gidMap = gidNameToNaturalMappings a
