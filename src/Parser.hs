module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

int :: Parser Expr
int = do
  n <- integer
  return $ IntType (fromInteger n)

bool :: Parser Expr
bool =
  reserved "true" >> return BoolType True
  <|>
  reserved "false" >> return BoolType False

text :: Parser Expr
text = do
  s <- string
  return $ TextType (T.pack s)

expr :: Parser Expr
  create <|> drop <|> select <|> insert <|> delete

-- create :: Parser Expr
-- create = do
--   reserved "create"
--   name <- identifier
--  --  
--   reservedOp ";"
--   return CREATE --

drop :: Parser Expr
drop = do
  reserved "drop"
  name <- identifier
  reservedOp ";"
  return Drop (Table name)

-- cond :: Parser Expr
-- cond = do
  
