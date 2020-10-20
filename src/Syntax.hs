module Syntax where

import qualified Data.Text as T

type Table = String

type FieldName = String

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

data ComparisonOp = Eq | Gt | Gte | Lt | Lte
  deriving (Show)

type Condition = (FieldName, ComparisonOp, FieldValue)

type IsPKey = Bool

data Column = Column IsPKey FieldName FieldType
  deriving (Show)

data Expr
  = Create Table [Column]
  | Drop Table
  | Select Table [Condition]
  | Insert Table [FieldValue]
  | Delete Table [Condition]
  deriving (Show)
