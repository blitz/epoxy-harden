{-# LANGUAGE DeriveGeneric #-}
module ApplicationDescription where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Maybe
import           GHC.Generics

data Process = Process { pid    :: Int,
                         binary :: FilePath }
  deriving (Show, Generic)

newtype ApplicationDescription = ApplicationDescription { processes :: [Process] }
  deriving (Show, Generic)

instance FromJSON Process
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
