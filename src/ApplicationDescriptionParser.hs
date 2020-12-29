module ApplicationDescriptionParser (parseApplicationDescription)
where

import           ApplicationDescription (ApplicationDescription)
import           Dhall                  (auto, inputFile)
import           Normalization
import           StackAllocation

parseApplicationDescription :: FilePath -> FilePath -> IO ApplicationDescription
parseApplicationDescription root f = do
  inputAppDesc <- inputFile auto f
  normalizedAppDesc <- normalizeApplicationDescription root inputAppDesc
  return $ allocateStacks normalizedAppDesc
