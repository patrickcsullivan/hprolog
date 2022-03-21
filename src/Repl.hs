module Repl (repl) where

import Control.Monad.IO.Class (liftIO)
import Parser (parsePredicate)
import Syntax (Rule)
import System.Console.Haskeline
  ( defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )

repl :: [Rule] -> IO ()
repl _program = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "?> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> liftIO (process input) >> loop

process :: String -> IO ()
process line = do
  let res = parsePredicate line
  case res of
    Left err -> print err
    Right rule -> print rule