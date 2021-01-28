module Main where

import Evaluator
import ExprParser
import Text.Parsec.Error
import Control.Monad
import Data.Maybe
import System.Console.Haskeline

main :: IO ()
main = do
  putStrLn logo
  runInputT defaultSettings loop
  where loop = do
          line <- getInputLine "cherry> "
          unless (isNothing line) $ do
            let r = readEval $ fromJust line
            outputStrLn $ case r of
              Right res -> res
              Left err -> show err
            loop

readEval :: String -> Either ParseError String
readEval s = do
  expr <- parseExpression s
  let result = eval expr
  return $ show result

logo :: String
logo = unlines ["","  /\\", " |  \\", " @   @",""]
