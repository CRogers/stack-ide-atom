module StackIdeSpec where

import Test.Hspec

should = it

spec :: Spec
spec =
  describe "StackIde should" $ do
    should "respond with the version when starting up" $ do
      True `shouldBe` False