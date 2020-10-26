module Database.Repl where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline
import System.IO (hFlush, stdout)

import Database.Main (Database, execExpr)
import qualified Database.Parser as P (parseExpr)

readInput :: IO String
readInput = putStr "> " >> hFlush stdout >> getLine

process :: Database -> String -> IO (Maybe Database)
process db expr = do
  let parsedExpr = P.parseExpr expr
  case parsedExpr of
    Left err -> print err >> return Nothing
    Right expr' -> do
      let newdb = execExpr db expr'
      return $ Just newdb

repl :: IO ()
repl = putStrLn ""

-- repl = runInputT defaultSettings (loop Placeholder)
--   where
--     loop db = do
--       input' <- readInput
--       case input' of
--         --Nothing -> liftIO $ outputStrLn "Quitting..."
--         "quit" -> putStrLn "Quitting..."
--         input -> do
--           newDb <- liftIO $ process db input
--           case newDb of
--             Nothing -> loop db
--             Just newDb -> loop newDb
