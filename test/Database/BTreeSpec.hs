module Database.BTreeSpec where

import qualified Database.BTree as BT
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
  describe "insert" $ do
    it True `shouldBe` True
