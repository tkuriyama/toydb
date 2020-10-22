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
    it "Single column" $
      let res = parse createP "" "Create MyTable [(True, MyField, Int)];"
       in res
            `shouldBe` ( Right (Create "MyTable" [(Column True "MyField" FtInt)])
                       )
    it "Multiple columns" $
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
  it "Missing Columns" $
    let res = parse createP "" "Create MyTable;"
     in res `shouldNotSatisfy` isRight
  it "Missing closing bracket" $
    let res = parse createP "" "Create MyTable [(True, MyField, Int);"
     in res `shouldNotSatisfy` isRight
  it "Missing semicolon" $
    let res = parse createP "" "Create MyTable [(True, MyField, Int)]"
     in res `shouldNotSatisfy` isRight

  describe "dropP" $ do
    it "Correct drop" $
      let res = parse dropP "" "Drop MyTable;"
       in res `shouldBe` (Right (Drop "MyTable"))
    it "Missing name" $
      let res = parse dropP "" "Drop;" in res `shouldNotSatisfy` isRight
    it "Missing semicolon" $
      let res = parse dropP "" "Drop MyTable"
       in res `shouldNotSatisfy` isRight

  describe "selectP" $ do
    it "Star" $
      let res = parse selectP "" "Select * From MyTable;"
       in res `shouldBe` (Right (Select AllFields "MyTable" []))
    it "Single column" $
      let res = parse selectP "" "Select [IntCol1] From MyTable;"
       in res `shouldBe` Right (Select (SomeFields ["IntCol1"]) "MyTable" [])
    it "Multiple columns with conditions" $
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
    it "Missing \"From\" keyword" $
      let res = parse selectP "" "Select [Col1] MyTable;"
       in res `shouldNotSatisfy` isRight
    it "Missing semicolon" $
      let res = parse selectP "" "Select * From MyTable"
       in res `shouldNotSatisfy` isRight

  describe "insertP" $ do
    it "Single column" $
      let res = parse insertP "" "Insert Into MyTable [1];"
       in res `shouldBe` (Right (Insert "MyTable" [(FvInt 1)]))
    it "Mutliple columns with each supported type" $
      let res = parse insertP "" "Insert Into MyTable [True, 1, \"Bob\"];"
       in res
            `shouldBe` ( Right
                           ( Insert
                               "MyTable"
                               [(FvBool True), (FvInt 1), (FvText "Bob")]
                           )
                       )
    it "Missing \"Into\" keyword" $
      let res = parse insertP "" "Insert MyTable [];"
       in res `shouldNotSatisfy` isRight
    it "Missing semicolon" $
      let res = parse insertP "" "Insert Into MyTable []"
       in res `shouldNotSatisfy` isRight

  describe "deleteP" $ do
    it "Single column" $
      let res =
            parse deleteP "" "Delete From MyTable Where [(IntCol, >, 1)];"
       in res
            `shouldBe` ( Right
                           (Delete "MyTable" [(Condition "IntCol" Gt (FvInt 1))])
                       )
    it "Multiple columns with each supported type" $
      let res =
            parse
              deleteP
              ""
              "Delete From MyTable Where [(IntCol, >, 1), (BoolCol, >=, True), (TextCol, =, \"Bob\")];"
       in res
            `shouldBe` Right
              ( Delete
                  "MyTable"
                  [ (Condition "IntCol" Gt (FvInt 1)),
                    (Condition "BoolCol" Gte (FvBool True)),
                    (Condition "TextCol" Eq (FvText "Bob"))
                  ]
              )
    it "Missing \"From\" keyword" $
      let res = parse selectP "" "Delete MyTable Where [(IntCol, >, 1)];"
       in res `shouldNotSatisfy` isRight
    it "Missing semicolon" $
      let res = parse selectP "" "Delete From MyTable Where [(IntCol, >, 1)]"
       in res `shouldNotSatisfy` isRight
