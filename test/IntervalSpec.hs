module IntervalSpec (spec) where

import           Test.Hspec

import qualified Interval   as I

zero :: Int
zero = 0

spec :: Spec
spec = do
  describe "fmap" $
    it "works on intervals" $
      fmap (+ 1) (I.Interval zero 10) `shouldBe` I.Interval 1 11
  describe "fromSize" $
    it "creates an interval of the given size" $
      I.size (I.fromSize zero 10) `shouldBe` 10
  describe "isEmpty" $
    it "is true for zero-sized intervals" $
      I.isEmpty (I.fromSize zero 0) `shouldBe` True
  describe "intersects" $ do
    it "is false for empty intervals" $
      I.intersects (I.fromSize zero 0) (I.fromSize 0 0) `shouldBe` False
    it "is false for non-intersecting intervals" $
      I.intersects (I.Interval zero 10) (I.Interval 20 30) `shouldBe` False
    it "is false for adjacent intervals" $
      I.intersects (I.Interval zero 10) (I.Interval 10 20) `shouldBe` False
    it "is true for partly overlapping intervals" $
      I.intersects (I.Interval zero 10) (I.Interval 5 15) `shouldBe` True
    it "is true when one interval encloses another" $
      I.intersects (I.Interval zero 10) (I.Interval 1 9) `shouldBe` True
  describe "isAdjacent" $ do
    it "is false for empty intervals" $
      I.isAdjacent (I.fromSize zero 0) (I.fromSize 0 0) `shouldBe` False
    it "is false for non-adjacent intervals" $
      I.isAdjacent (I.Interval zero 10) (I.Interval 20 30) `shouldBe` False
    it "is true for adjacent intervals" $
      I.isAdjacent (I.Interval zero 10) (I.Interval 10 20) `shouldBe` True
  describe "joinAdjacent" $ do
    it "joins adjacent intervals" $
      I.joinAdjacent (I.Interval zero 10) (I.Interval 10 20) `shouldBe` I.Interval 0 20
    it "joins intersecting intervals" $
      I.joinAdjacent (I.Interval zero 15) (I.Interval 10 20) `shouldBe` I.Interval 0 20
  describe "subtract" $ do
    it "doesn't do anything for non-intersecting intervals" $
      I.subtract (I.Interval zero 10) (I.Interval 20 3) `shouldBe` [I.Interval 0 10]
    it "returns a single result for partly overlapping intervals" $
      I.subtract (I.Interval zero 10) (I.Interval 5 20) `shouldBe` [I.Interval 0 5]
    it "returns two results when the second interval is inside the first" $
      I.subtract (I.Interval zero 10) (I.Interval 5 6) `shouldBe` [I.Interval 0 5, I.Interval 6 10]
