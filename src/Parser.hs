module Parser where

import qualified Lexer as L
import Syntax

import qualified Data.Text as T

import Text.Parsec (many, ParseError, parse, (<|>))
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

-- Create
-- Create MyTable [(False, MyFieldName, Bool), (True, MyPrimaryFieldname, Int)]

create :: Parser Expr
create = do
  L.reserved "Create"
  name <- L.identifier
  columns <- L.brackets $ L.commaSep $ L.parens column
  L.semi
  return $ Create name columns

column :: Parser Column
column = f <$> pkey <*> L.comma <*> fieldName <*> L.comma <*> fieldType
  where f a _ b _ c = Column a b c

-- Drop
-- Drop MyTable

dropP :: Parser Expr
dropP = do
  L.reserved "Drop"
  name <- L.identifier
  L.semi
  return $ Drop name

-- Select
-- Select From MyTable...


-- Insert
-- Insert Into MyTable ...


-- Delete
-- Delete From MyTable...


-- FIeld Name, Types and Values

fieldName :: Parser FieldName
fieldName = do
  n <- L.identifier
  return $ n

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
  return $ FtBool

intType :: Parser FieldType
intType = do
  L.reserved "Int"
  return $ FtInt

textType :: Parser FieldType
textType = do
  L.reserved "Text"
  return $ FtText

-- Other Primitives 

pkey :: Parser IsPKey
pkey = f <$> boolValue
  where
    f (FvBool True) = True
    f (FvBool False) = False

-- Run Parser

parseToplevel :: (Show a) => Parser a -> String -> Either ParseError a
parseToplevel p s = parse p "<stdin>" s

process :: (Show a) => Parser a -> String -> IO ()
process p line = do
  let res = parseToplevel p line
  case res of
    Left err -> print err
    Right s -> print s
