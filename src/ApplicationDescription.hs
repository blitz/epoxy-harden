{-# LANGUAGE DeriveGeneric #-}
module ApplicationDescription where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Maybe
import           GHC.Generics

data KObject = KObject { gid      :: Int,
                         kobjType :: String }
  deriving (Show, Generic)

data Process = Process { pid          :: Int,
                         binary       :: FilePath,
                         capabilities :: [Int] }
  deriving (Show, Generic)

data ApplicationDescription = ApplicationDescription
  { kobjects  :: [KObject],
    processes :: [Process] }
  deriving (Show, Generic)

instance FromJSON Process
instance FromJSON KObject
instance FromJSON ApplicationDescription

verify :: [Process] -> Bool
verify p = all (uncurry (==)) (zip [0..] (map pid p))

parseApplicationDescription :: FilePath -> IO ApplicationDescription
parseApplicationDescription file = do
  desc <- decode <$> B.readFile file
  if isJust desc && verify (processes (fromJust desc)) then
    return (fromJust desc)
  else
    fail "Failed to parse application JSON file"
