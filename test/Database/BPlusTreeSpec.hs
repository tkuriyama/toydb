module Database.BPlusTreeSpec where

import Database.BPlusTree
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

spec :: Spec
spec = do
  it "always true" $ True == True
