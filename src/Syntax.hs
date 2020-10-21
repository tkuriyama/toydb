module Syntax where

import qualified Data.Text as T

-- Everything is an expression; no recursion required
data Expr
  = Create Table [Column]
  | Drop Table
  | Select Table [Condition]
  | Insert Table [FieldValue]
  | Delete Table [Condition]
  deriving (Show)

data Column = Column IsPKey FieldName FieldType
  deriving (Show)

type IsPKey = Bool

type Table = String

data Condition = Condition FieldName ComparisonOp FieldValue
  deriving (Show)

type FieldName = String

data ComparisonOp = Eq | Gt | Gte | Lt | Lte
  deriving (Show)

data FieldValue
  = FvInt Integer
  | FvText T.Text
  | FvBool Bool
  deriving (Show)

data FieldType
  = FtInt
  | FtText
  | FtBool
  deriving (Show)



