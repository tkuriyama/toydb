module Parser where

import qualified Data.Text as T
import Lexer
import Syntax
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

intValue :: Parser FieldValue
intValue = do
  n <- integer
  return $ FvInt (fromInteger n)

textValue :: Parser FieldValue
textValue = do
  s <- string'
  return $ FvText (T.pack s)

boolValue :: Parser FieldValue
boolValue = boolTrue <|> boolFalse

boolTrue :: Parser FieldValue
boolTrue = do
  reserved "true"
  return $ FvBool True

boolFalse :: Parser FieldValue
boolFalse = do
  reserved "false"
  return $ FvBool False

fieldType :: Parser FieldType
fieldType = boolType <|> intType <|> textType

intType :: Parser FieldType
intType = do
  reserved "Int"
  return $ FtInt

textType :: Parser FieldType
textType = do
  reserved "Text"
  return $ FtText

boolType :: Parser FieldType
boolType = do
  reserved "Bool"
  return $ FtBool

pkey :: Parser IsPKey
pkey = f <$> boolValue
  where
    f (FvBool True) = True
    f (FvBool False) = False

fieldName :: Parser FieldName
fieldName = do
  n <- string'
  return $ n

{-expr ::
  Parser
    Expr
    create
    <|> drop
    <|> select
    <|> insert
    <|> delete
-}

commaParser :: Parser ()
commaParser = do
  reservedOp ","
  return $ ()

column :: Parser Syntax.Column
column = f <$> pkey <*> commaParser <*> fieldName <*> commaParser <*> fieldType
  where
    f a _ b _ c = Column a b c

-- CREATE MyTable [(False, MyFieldName, Bool), (True, MyPrimaryFieldname, Int)]
create :: Parser Expr
create = do
  reserved "create"
  name <- identifier
  columns <- brackets $ many $ parens column
  reservedOp ";"
  return $ Create name columns

drop :: Parser Expr
drop = do
  reserved "drop"
  name <- identifier
  reservedOp ";"
  return $ Drop name

-- cond :: Parser Expr
-- cond = do
parseToplevel :: String -> Either ParseError Expr
parseToplevel s = parse Parser.create "<stdin>" s

process :: String -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> print err
    Right ex -> print ex
