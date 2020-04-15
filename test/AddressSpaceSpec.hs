module AddressSpaceSpec (spec) where

import qualified Data.ByteString           as B
import           Data.Elf
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           AddressSpace

givenSegments    = [ ElfSegment PT_LOAD [] 0x10000 0x10000 0x1000 (B.replicate 0x530 0) 0x530,
                     ElfSegment PT_LOAD [] 0x11530 0x11530 0x1000 (B.replicate 0x798 0) 0x818 ]

expectedSegments = [ ElfSegment PT_LOAD [] 0x10000 0x10000 0x1000 (B.replicate 0x530 0) 0x530,
                     ElfSegment PT_LOAD [] 0x11000 0x11000 0x1000 (B.replicate (0x798 + 0x530) 0) (0x818 + 0x530) ]

spec :: Spec
spec =
  describe "bootElfFromMemory" $ do
    it "creates an empty ELF from empty memory" $
      -- TODO If this check fails, the test keeps allocating memory
      -- while trying to print what's going wrong.
      zeroPadToPage <$> givenSegments `shouldBe` expectedSegments
