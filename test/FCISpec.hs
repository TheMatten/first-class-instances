module FCISpec (spec) where

import Test.Hspec

import FCI

spec :: Spec
spec = do
  describe "inst" do
    it "can capture normal instances" do
      _fmap (inst @(Functor Maybe)) (+1) (Just (1 :: Int)) `shouldBe` Just 2

  describe "==>" do
    it "can provide custom instances" do
      coerceFunctor @FillTest ==>
        fmap (+1) (FillTest (1 :: Int)) `shouldBe` FillTest 2

newtype FillTest a = FillTest a deriving (Eq, Show)
