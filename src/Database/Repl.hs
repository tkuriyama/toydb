module Database.Repl where

import Control.Monad (unless)
import Main
  ( Database,
    execExpr,
  )
import System.Console.Haskeline
import System.IO
  ( hFlush,
    stdout,
  )

readInput :: IO String
readInput = putStr "> " >> hFlush stdout >> getLine

process :: Database -> String -> IO (Maybe Database)
process db expr = do
  let parsedExpr = parseExpr expr
  case parsedExpr of
    Left err -> print err >> return Nothing
    Right expr -> do
      newdb <- execExpr db expr
      return $ Just newDb

repl :: IO
repl = runInputT defaultSettings (loop Placeholder)
  where
    loop db = do
      input' <- readInput
      case input' of
        Nothing -> outputStrLn "Quitting..."
        Just input -> do
          newDb <- liftIO $ process db input
          case newDb of
            Nothing -> loop db
            Just newDb -> loop newDb
