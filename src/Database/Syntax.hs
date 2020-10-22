module Database.Syntax where

import qualified Data.Text as T

-- Everything is an expression; no recursion required
data Expr
  = Create Table [Column]
  | Drop Table
  | Select FieldSelect Table [Condition]
  | Insert Table [FieldValue]
  | Delete Table [Condition]
  deriving (Show, Eq)

data FieldSelect
  = SomeFields [FieldName]
  | AllFields
  deriving (Show, Eq)

data Column = Column IsPKey FieldName FieldType
  deriving (Show, Eq)

type IsPKey = Bool

type Table = String

data Condition = Condition FieldName ComparisonOp FieldValue
  deriving (Show, Eq)

type FieldName = String

data ComparisonOp = Eq | Gt | Gte | Lt | Lte
  deriving (Show, Eq)

data FieldValue
  = FvInt Integer
  | FvText T.Text
  | FvBool Bool
  deriving (Show, Eq)

data FieldType
  = FtInt
  | FtText
  | FtBool
  deriving (Show, Eq)
