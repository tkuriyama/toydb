module Database.Main where

import Database.Repl (repl)

import Database.Syntax
import Database.Parser

import Data.Text as T

data Database = Placeholder

execExpr :: Database -> Expr -> Database
execExpr db expr =
  case expr of
    (Create tbl cols) -> createTable db tbl cols
    (Drop tbl) -> dropTable db tbl
    (Select fs tbl conds) -> select db fs tbl conds
    (Insert tbl fvs) -> insert db tbl fvs
    (Delete tbl conds) -> delete db tbl conds

createTable :: Database -> Table -> [Column] -> Database
createTable = undefined

dropTable :: Database -> Table -> Database
dropTable = undefined      

select :: Database -> FieldSelect -> Table -> [Condition] -> Database
select = undefined

insert :: Database -> Table -> [FieldValue] -> Database
insert = undefined

delete :: Database -> Table -> [Condition] -> Database
delete = undefined      

main :: IO ()
main = repl
