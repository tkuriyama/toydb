module Syntax where

import qualified Data.Text as T

type Table = String
type FieldName = String
type FieldValue = String
data FieldType = IntType Int
               | TextType T.Text
               | BoolType Bool

data ComparisonOp = Eq | Gt | Gte | Lt | Lte
type Condition = (FieldName, ComparisonOp, FieldValue)

type IsPKey = Bool
data Column = Column IsPKey FieldName FieldType 

data Expr = Create Table [Column]
          | Drop Table
          | Select Table [Condition]
          | Insert Table [FieldValue]
          | Delete Table [Condition]

