module Database.Parser where

import qualified Data.Text as T
import qualified Database.Lexer as L
import Database.Syntax
import Text.Parsec
  ( ParseError,
    many,
    option,
    parse,
    try,
    (<|>),
  )
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

-- Create
-- Create MyTable [(False, MyFieldName, Bool), (True, MyPrimaryFieldname, Int)];

createP :: Parser Expr
createP = do
  L.reserved "Create"
  name <- L.identifier
  columns <- L.brackets $ L.commaSep $ L.parens column
  L.semi
  return $ Create name columns

column :: Parser Column
column = f <$> pkey <*> L.comma <*> fieldName <*> L.comma <*> fieldType
  where
    f a _ b _ c = Column a b c

-- Drop
-- Drop MyTable

dropP :: Parser Expr
dropP = do
  L.reserved "Drop"
  name <- L.identifier
  L.semi
  return $ Drop name

-- Select
-- Select * From MyTable;
-- Select [IntCol1, TxtCol1] From MyTable Where [(IntCol1, >, 1), (TxtCol1, =, \"Bob\")];

selectP :: Parser Expr
selectP = do
  L.reserved "Select"
  fields <- fieldSelect
  L.reserved "From"
  name <- L.identifier
  conds <- option [] conditions
  L.semi
  return $ Select fields name conds

fieldSelect :: Parser FieldSelect
fieldSelect = star <|> fieldNames
  where
    fieldNames = SomeFields <$> L.brackets (L.commaSep fieldName)
    star = do
      L.reservedOp "*"
      return AllFields

conditions :: Parser [Condition]
conditions = do
  L.reserved "Where"
  conds <- L.brackets $ L.commaSep $ L.parens condition
  return conds

condition :: Parser Condition
condition = f <$> fieldName <*> L.comma <*> operator <*> L.comma <*> fieldValue
  where
    f a _ b _ c = Condition a b c

operator :: Parser ComparisonOp
operator = eqOp <|> gtOp <|> gteOp <|> ltOp <|> lteOp
  where
    eqOp = do
      L.reservedOp "="
      return Eq
    gtOp = do
      L.reservedOp ">"
      return Gt
    gteOp = do
      L.reservedOp ">="
      return Gte
    ltOp = do
      L.reservedOp "<"
      return Lt
    lteOp = do
      L.reservedOp "<="
      return Lte

-- Insert
-- Insert Into MyTable [1, \"Bob"\, True];

insertP :: Parser Expr
insertP = do
  L.reserved "Insert"
  L.reserved "Into"
  name <- L.identifier
  fvs <- L.brackets $ L.commaSep fieldValue
  L.semi
  return $ Insert name fvs

-- Delete
-- Delete From MyTable Where [(IntCol1, >, 1), (TxtCol1, =, \"Bob\")];

deleteP :: Parser Expr
deleteP = do
  L.reserved "Delete"
  L.reserved "From"
  name <- L.identifier
  conds <- conditions
  L.semi
  return $ Delete name conds

-- Field Name, Types and Values

fieldName :: Parser FieldName
fieldName = do
  n <- L.identifier
  return n

fieldValue :: Parser FieldValue
fieldValue = boolValue <|> intValue <|> textValue

boolValue :: Parser FieldValue
boolValue = boolTrue <|> boolFalse
  where
    boolTrue :: Parser FieldValue
    boolTrue = do
      L.reserved "True"
      return $ FvBool True
    boolFalse :: Parser FieldValue
    boolFalse = do
      L.reserved "False"
      return $ FvBool False

intValue :: Parser FieldValue
intValue = do
  n <- L.integer
  return $ FvInt (fromInteger n)

textValue :: Parser FieldValue
textValue = do
  s <- L.string
  return $ FvText (T.pack s)

fieldType :: Parser FieldType
fieldType = boolType <|> intType <|> textType

boolType :: Parser FieldType
boolType = do
  L.reserved "Bool"
  return FtBool

intType :: Parser FieldType
intType = do
  L.reserved "Int"
  return FtInt

textType :: Parser FieldType
textType = do
  L.reserved "Text"
  return FtText

-- Other Primitives

pkey :: Parser IsPKey
pkey = f <$> boolValue
  where
    f (FvBool True) = True
    f (FvBool False) = False

-- Top level expression parser

exprP :: Parser Expr
exprP =
  try selectP <|> try dropP <|> try createP <|> try insertP <|> try deleteP

parseExpr :: String -> Either ParseError Expr
parseExpr = parse exprP ""

-- Run Parser
process :: String -> IO ()
process line = do
  let res = parse exprP "" line
  case res of
    Left err -> print err
    Right s -> print s
