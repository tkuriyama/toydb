{-# LANGUAGE OverloadedStrings #-}

module Database.Repl where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Database.Main
  ( Database,
    execExpr,
  )
import qualified Database.Parser as P
  ( parseExpr,
  )
import System.Console.Haskeline

process :: Database -> String -> IO (Maybe Database)
process db expr = do
  let parsedExpr = P.parseExpr expr
  case parsedExpr of
    Left err -> print err >> return Nothing
    Right expr' -> do
      case execExpr db expr' of
        Left err -> do
          print err
          return Nothing
        Right (newdb, msg) -> do
          print msg
          return $ Just newdb

repl :: IO ()
repl = runInputT defaultSettings (loop Map.empty)
  where
    loop db = do
      input' <- getInputLine "> "
      case input' of
        Nothing -> outputStrLn "Quitting..."
        Just input -> do
          newDb' <- liftIO $ process db input
          case newDb' of
            Nothing -> loop db
            Just newDb -> loop newDb
