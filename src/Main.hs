module Main where

import Evaluator
import Enviroment
import Parser
import Control.Monad
import Data.Maybe
import System.Console.Haskeline
import Control.Monad.State

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
            let (out, nEnv) = case expr of
                  (Left err)    -> (show err, env)
                  (Right expr') -> let (nExp, env') = runState (evalS expr') env
                                   in (show nExp, env')
            outputStrLn out
            repl' nEnv

logo :: String
logo = unlines ["","  /\\", " |  \\", " @   @",""]
