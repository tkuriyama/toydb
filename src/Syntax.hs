module Syntax where

type Table = String
type FieldName = String
type FieldValue = String
data FieldType = IntType | TextType | BoolType

type ComparisonOp = Eq | Gt | Gte | Lt | Lte
type Condition = (FieldName, ComparisonOp, FieldValue)

type IsPKey = Bool
data Column = Column IsPKey FieldName FieldType 

data Expr = Create Table [Column]
          | Drop Table
          | Select Table [Condition]
          | Insert Table [FieldValue]
          | Delete Table [Condition]

