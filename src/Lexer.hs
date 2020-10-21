module Lexer where

import qualified Data.Text as T
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["=", ">", ">=", "<", "<=", "*"]
    names =
      [ "Create",
        "Drop",
        "Select",
        "From",
        "Insert",
        "Into",
        "Delete",
        "True",
        "False",
        "Int",
        "Bool",
        "Text"
      ]
    style =
      emptyDef
        { Tok.commentLine = "--",
          Tok.reservedOpNames = ops,
          Tok.reservedNames = names
        }

integer :: Parser Integer
integer = Tok.integer lexer

string :: Parser String
string = Tok.stringLiteral lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

identifier :: Parser String
identifier = Tok.identifier lexer

comma :: Parser String
comma = Tok.comma lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semi :: Parser String
semi = Tok.semi lexer


reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
