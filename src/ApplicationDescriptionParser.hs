module ApplicationDescriptionParser (parseApplicationDescription)
where

import           ApplicationDescription (ApplicationDescription)
import           Dhall                  (auto, inputFile)
import           Normalization
import           StackAllocation

parseApplicationDescription :: FilePath -> IO ApplicationDescription
parseApplicationDescription f = do
  inputAppDesc <- inputFile auto f
  normalizedAppDesc <- normalizeApplicationDescription inputAppDesc
  return $ allocateStacks normalizedAppDesc
