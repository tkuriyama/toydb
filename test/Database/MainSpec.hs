{-# LANGUAGE OverloadedStrings #-}

module Database.MainSpec where

import qualified Data.Map as Map
import Database.BPlusTree
import Database.Main
import Database.Syntax
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

isRight :: Either e a -> Bool
isRight result = case result of
  Left err -> False
  Right _ -> True

spec :: Spec
spec = do
  describe "Verification" $ do
    it "Int type verify" $ verifyType FtInt (FvInt 1) `shouldBe` True
    it "Text type verify" $ verifyType FtText (FvText "") `shouldBe` True
    it "Bool type verify" $ verifyType FtBool (FvBool True) `shouldBe` True
    it "verify doesn't pass Int == Bool" $
      verifyType FtInt (FvBool True)
        `shouldBe` False
    it "verify doesn't pass Int == Text" $
      verifyType FtInt (FvText "")
        `shouldBe` False
    it "verify doesn't pass Text == Bool" $
      verifyType FtText (FvBool True)
        `shouldBe` False
    it "verify list of correct types" $
      verifyTypes
        ( Schema
            ""
            ( Map.insert
                "1"
                FtInt
                (Map.insert "2" FtBool (Map.insert "3" FtText Map.empty))
            )
        )
        [(FvInt 1), (FvBool True), (FvText "")]
        `shouldBe` True
    it "verify list of incorrect types" $
      verifyTypes
        ( Schema
            ""
            ( Map.insert
                "1"
                FtInt
                (Map.insert "2" FtBool (Map.insert "3" FtText Map.empty))
            )
        )
        [(FvInt 1), (FvInt 1), (FvText "")]
        `shouldBe` False
    it "verifySchema with single pkey" $
      verifySchema [(Column True "" FtInt), (Column False "" FtInt)]
        `shouldBe` True
    it "verifySchema with no pkey" $
      verifySchema [(Column False "" FtInt), (Column False "" FtInt)]
        `shouldBe` False
    it "verifySchema with muliplt pkey" $
      verifySchema [(Column True "" FtInt), (Column True "" FtInt)]
        `shouldBe` False
    it "verifyTableName with new unique name" $
      verifyTableName Map.empty "a"
        `shouldBe` True
    it "verifyTableName with exisiting name" $
      verifyTableName
        ( Map.insert
            "a"
            (Table (Schema "" Map.empty) (BPTree Nil Map.empty 1))
            Map.empty
        )
        "a"
        `shouldBe` False
