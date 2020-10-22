{-# LANGUAGE OverloadedStrings #-}

module Database.ParserSpec where

import Database.Parser
import Database.Syntax
import Test.Hspec
  ( Spec,
    describe,
    hspec,
    it,
    shouldBe,
    shouldNotSatisfy,
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))
import Text.Parsec (parse)

isRight :: Either e a -> Bool
isRight result = case result of
  Left err -> False
  Right _ -> True

spec :: Spec
spec = do
  describe "createP" $ do
    it "Single Column Table" $
      let res = parse createP "" "Create MyTable [(True, MyField, Int)];"
       in res
            `shouldBe` ( Right (Create "MyTable" [(Column True "MyField" FtInt)])
                       )
    it "Multiple Column Table" $
      let res =
            parse
              createP
              ""
              "Create MyTable [(True, MyField, Int), (False, MyOtherField, Text), (False, MyThirdField, Bool)];"
       in res
            `shouldBe` ( Right
                           ( Create
                               "MyTable"
                               [ (Column True "MyField" FtInt),
                                 (Column False "MyOtherField" FtText),
                                 (Column False "MyThirdField" FtBool)
                               ]
                           )
                       )
  it "No Columns" $
    let res = parse createP "" "Create MyTable;"
     in res `shouldNotSatisfy` isRight
  it "Missing closing bracket" $
    let res = parse createP "" "Create MyTable [(True, MyField, Int);"
     in res `shouldNotSatisfy` isRight

  describe "dropP" $ do
    it "Correct drop" $
      let res = parse dropP "" "Drop MyTable;"
       in res `shouldBe` (Right (Drop "MyTable"))
    it "Missing name" $
      let res = parse dropP "" "Drop;" in res `shouldNotSatisfy` isRight

  describe "SelectP" $ do
    it "Select Star" $
      let res = parse selectP "" "Select * From MyTable;"
       in res `shouldBe` (Right (Select AllFields "MyTable" []))
    it "Select one column" $
      let res = parse selectP "" "Select [IntCol1] From MyTable;"
       in res `shouldBe` Right (Select (SomeFields ["IntCol1"]) "MyTable" [])
    it "Select columns with conditions" $
      let res =
            parse
              selectP
              ""
              "Select [IntCol1, TxtCol1] From MyTable Where [(IntCol1, >, 1), (TxtCol1, =, \"Bob\")];"
       in res
            `shouldBe` Right
              ( Select
                  (SomeFields ["IntCol1", "TxtCol1"])
                  "MyTable"
                  [ (Condition "IntCol1" Gt (FvInt 1)),
                    (Condition "TxtCol1" Eq (FvText "Bob"))
                  ]
              )
