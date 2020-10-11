module ElfWriterSpec (spec) where

import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as BL
import           Data.Elf
import           Data.Int                  (Int64)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           PhysMem
import           Writer

segOneData :: BL.ByteString
segOneData = BL.pack [0, 0, 0]

segTwoData :: BL.ByteString
segTwoData = BL.pack [4, 5, 6, 0, 0]

exampleMemory :: Memory
exampleMemory = writeMemory 10 segOneData (writeMemory 20 segTwoData emptyMemory)

allSegmentData :: ElfSegment -> B.ByteString
allSegmentData seg = B.append segData (B.replicate (fromIntegral (elfSegmentMemSize seg) - B.length segData) 0)
  where segData = elfSegmentData seg

bootElfFromMemory :: Int64 -> Memory -> B.ByteString
bootElfFromMemory = resolveWriterFunction "ELF"

spec :: Spec
spec =
  describe "bootElfFromMemory" $ do
    it "creates an empty ELF from empty memory" $
      null $ elfSegments $ parseElf $ bootElfFromMemory 0 emptyMemory
    it "sets the entry point" $
      property $ \entry -> (elfEntry $ parseElf $ bootElfFromMemory entry emptyMemory) `shouldBe` fromIntegral entry
    it "creates one segment per chunk of memory" $
      length (elfSegments $ parseElf $ bootElfFromMemory 0 exampleMemory) == 2
    it "stores segment data (1)" $
      (allSegmentData <$> (elfSegments $ parseElf $ bootElfFromMemory 0 exampleMemory)) `shouldBe` (BL.toStrict <$> [segOneData, segTwoData])
    it "doesn't write trailing zeroes into the ELF" $
      (elfSegmentData <$> (elfSegments $ parseElf $ bootElfFromMemory 0 exampleMemory)) `shouldBe` (BL.toStrict . BL.pack <$> [[], [4, 5, 6]])
    it "sets physical addresses" $
      property $ \entry -> (elfSegmentPhysAddr <$> (elfSegments $ parseElf $ bootElfFromMemory 0 (writeMemory entry segOneData emptyMemory))) `shouldBe` [fromIntegral entry]
