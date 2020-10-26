module Main where

import qualified Database.Main as DM
import qualified Database.Repl as REPL

main :: IO ()
main = REPL.repl
