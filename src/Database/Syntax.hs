module Database.Syntax where

import qualified Data.Text as T

-- Everything is an expression; no recursion required
data Expr
  = Create TableName [Column]
  | Drop TableName
  | Select FieldSelect TableName [Condition]
  | Insert TableName [FieldValue]
  | Delete TableName [Condition]
  deriving (Show, Eq)

data FieldSelect
  = SomeFields [FieldName]
  | AllFields
  deriving (Show, Eq)

data Column = Column {colIsPkey :: IsPKey, colFieldName :: FieldName, colFieldType :: FieldType}
  deriving (Show, Eq)

type IsPKey = Bool

type TableName = String

data Condition
  = Condition FieldName ComparisonOp FieldValue
  deriving (Show, Eq)

type FieldName = String

data ComparisonOp
  = Eq
  | Gt
  | Gte
  | Lt
  | Lte
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
