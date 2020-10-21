module Database.Database where

import Database.Syntax

import Data.Text as T

data Database = TBD

execExpr :: Database -> Expr -> Database
execExpr db expr =
  case expr of
    (Create tbl cols) -> createTable db tbl cols
    (Drop tbl) -> dropTable db tbl
    (Select tbl conds) -> select db tbl conds
    (Insert tbl fvs) -> insert db tbl fvs
    (Delete tbl conds) -> delete db tbl conds

createTable :: Database -> Table -> [Column] -> Database
createTable = undefined

dropTable :: Database -> Table -> Database
dropTable = undefined      

select :: Database -> Table -> [Condition] -> Database
select = undefined

insert :: Database -> Table -> [FieldValue] -> Database
insert = undefined

delete :: Database -> Table -> [Condition] -> Database
delete = undefined      
