module PhysMemSpec (spec) where

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Int                   (Int64)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances  ()

import qualified Interval                   as I
import           PhysMem

readWriteMemory :: Int64 -> BL.ByteString -> Memory -> BL.ByteString
readWriteMemory start string =
  readMemory ivl . writeMemory start string
  where ivl = I.fromSize start (fromIntegral (BL.length string))

spec :: Spec
spec = do
  describe "readMemory" $ do
    it "reads empty memory as zeros" $
      readMemory (I.fromSize 10 30) [] `shouldBe` sameByte 30 0
    it "reads partial memory chunks" $
      readMemory (I.fromSize 1 1) (writeMemory 0 (B8.pack "Bar") []) `shouldBe` B8.pack "a"
    it "reads partially written memory" $
      readMemory (I.fromSize 0 3) (writeMemory 1 (B8.pack "!") []) `shouldBe` B8.pack "\0!\0"
    it "reads multiple adjacent regions as one" $
      readMemory (I.fromSize 0 6) (writeMemory 3 (B8.pack "Bar")
                                    (writeMemory 0 (B8.pack "Foo") [])) `shouldBe` B8.pack "FooBar"
    it "reads multiple overlapping regions as one" $
      readMemory (I.fromSize 0 6) (writeMemory 3 (B8.pack "Bar") (writeMemory 0 (B8.pack "FooFob") [])) `shouldBe` B8.pack "FooBar"
  describe "writeMemory" $
    it "is inverse to readMemory" $ property $ \s x -> readWriteMemory s x [] == x
  describe "flatten" $ do
    it "doesn't change the content" $
      readMemory (I.fromSize 0 6) (writeMemory 0 (B8.pack "Foo")
                                   (writeMemory 3 (B8.pack "Bar") [])) `shouldBe` B8.pack "FooBar"
    it "joins two adjacent memory chunks" $
      length (flatten (writeMemory 0 (B8.pack "Foo")
                        (writeMemory 3 (B8.pack "Bar") []))) `shouldBe` 1
    it "does not join non-adjacent regions" $
      length (flatten (writeMemory 0 (B8.pack "Foo")
                        (writeMemory 4 (B8.pack "Bar") []))) `shouldBe` 2
    it "joins overlapping regions" $
      length (flatten (writeMemory 0 (B8.pack "Foo")
                        (writeMemory 2 (B8.pack "Bar") []))) `shouldBe` 1
