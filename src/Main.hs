module Main where

import Evaluator
import ExprParser
import Text.Parsec.Error
import Data.Either
import System.IO

main :: IO ()
main = do
  putStrLn ""
  putLogo
  putStrLn ""
  cycle
  where cycle = do
          putPrompt
          hFlush stdout
          s <- getLine
          let r = readEval s
          putStrLn $ case r of
            (Right res) -> res
            (Left err) -> show err
          cycle

readEval :: String -> Either ParseError String
readEval s = do
  expr <- parseExpression s
  let result = eval expr
  return $ show result

putLogo :: IO ()
putLogo = putStr . concat $ map (++"\n") ["  /\\", " |  \\", " @   @"]

putPrompt :: IO ()
putPrompt = putStr "chery> "
