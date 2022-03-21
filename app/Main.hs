module Main where

import Parser (parseProgram)
import Repl (repl)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    path : _ -> do
      file <- openFile path ReadMode
      contents <- hGetContents file
      case parseProgram contents of
        Left err -> print err
        Right program -> repl program
    [] ->
      putStrLn "<INPUT> argument required"
