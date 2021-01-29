module Main where

import Evaluator
import ExprParser
import Control.Monad
import Data.Maybe
import System.Console.Haskeline

main :: IO ()
main = do
  putStrLn logo
  runInputT defaultSettings repl

repl :: InputT IO ()
repl = do
  repl' base
  where repl' env = do
          line <- getInputLine "cherry> "
          unless (isNothing line) $ do
            let expr = parseExpression $ fromJust line
            let (nEnv, out) = case expr of
                  (Left err)    -> (env, show err)
                  (Right expr') -> let (env', nExp) = eval env expr'
                                   in (env', show nExp)
            outputStrLn out
            repl' nEnv

logo :: String
logo = unlines ["","  /\\", " |  \\", " @   @",""]
