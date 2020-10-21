module Database.Main where

import Database.Repl (repl)
import Database.Parser

main :: IO ()
main = repl
