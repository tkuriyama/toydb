{-# LANGUAGE OverloadedStrings #-}

module Database.Main where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Database.BPlusTree
import Database.Parser
import Database.Syntax

data Schema
  = Schema Pkey (Map.Map String FieldType)
  deriving (Show)

data Table = Table
  { s :: Schema,
    values :: BPTree FieldType FieldValue
  }
  deriving (Show)

type Pkey = String

type Database = Map.Map String Table

type ErrorMsg = String

type ExprResult = Either ErrorMsg (Database, String)

placeholderfunc :: ExprResult
placeholderfunc = Right (Map.empty, "")

-- Verification Functions
verifyType :: FieldType -> FieldValue -> Bool
verifyType FtInt (FvInt _) = True
verifyType FtBool (FvBool _) = True
verifyType FtText (FvText _) = True
verifyType _ _ = False

verifyTypes :: Schema -> [FieldValue] -> Bool
verifyTypes (Schema _ colTypes) vals =
  foldr
    (\(col, val) acc -> acc && verifyType col val)
    True
    (zip (map snd (Map.toList colTypes)) vals)

verifySchema :: [Column] -> Bool
verifySchema cols = length (filter colIsPkey cols) == 1

verifyTableName :: Database -> TableName -> Bool
verifyTableName db name = case Map.lookup name db of
  Just _ -> False
  Nothing -> True

-- TODO: Better way to do this?
findPkey :: [Column] -> String
findPkey ((Column True name _) : _) = name
findPkey ((Column False _ _) : cols) = findPkey cols

-- Table creation Functions
createSchema :: [Column] -> Schema
createSchema cols = Schema schemaPkey colMap
  where
    colMap =
      foldr
        (\col accMap -> Map.insert (colFieldName col) (colFieldType col) accMap)
        Map.empty
        cols
    schemaPkey = findPkey cols

-- Expression driver functions
execExpr :: Database -> Expr -> ExprResult
execExpr db expr = case expr of
  (Create tbl cols) -> createTable db tbl cols
  (Drop tbl) -> dropTable db tbl
  (Select fs tbl conds) -> select db fs tbl conds
  (Insert tbl fvs) -> insertx db tbl fvs
  (Delete tbl conds) -> delete db tbl conds

-- TODO: Refactor nested case statements
createTable :: Database -> TableName -> [Column] -> ExprResult
createTable db name cols = case verifySchema cols of
  False -> Left "Invalid number of primary keys, only one is allowed"
  True -> case verifyTableName db name of
    False -> Left "Invalid table name, table already exists"
    True -> Right (Map.insert name table db, "Successfully created table")
      where
        schema = createSchema cols
        table = Table schema (BPTree Nil Map.empty 1)

dropTable :: Database -> TableName -> ExprResult
dropTable db name = case verifyTableName db name of
  False -> Left "Table doesn't exist"
  True -> Right (Map.delete name db, "Successfully deleted table")

select :: Database -> FieldSelect -> TableName -> [Condition] -> ExprResult
select db fs tbl conds = placeholderfunc

insertx :: Database -> TableName -> [FieldValue] -> ExprResult
insertx db tbl fvs = placeholderfunc

delete :: Database -> TableName -> [Condition] -> ExprResult
delete db tbl conds = placeholderfunc
