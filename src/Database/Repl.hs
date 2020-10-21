module Database.Repl where

import Control.Monad (unless)
import System.IO (stdout, hFlush)

readInput :: IO String
readInput = putStr "> "
            >> hFlush stdout
            >> getLine

echo :: String -> IO ()
echo = putStrLn 

evalInput :: String -> String
evalInput = id

repl :: IO ()
repl = do
  input <- readInput
  
  unless (input == ":quit")
       $ echo (evalInput input) >> repl
