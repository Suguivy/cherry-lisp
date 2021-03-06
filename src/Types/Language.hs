module Types.Language where

import Data.Map (Map)
import Control.Monad.State

data Enviroment = Enviroment (Map String Expr) (Maybe Enviroment)

data Expr = IntE Integer
          | VarE String
          | SetE String Expr
          | ConsE Expr Expr
          | LambdaE String Expr
          | BuiltinProcE ([Expr] -> State Enviroment Expr)
          | QuotedE Expr
          | NilE

instance Show Expr where
  show (IntE x) = show x
  show (VarE x) = x
  show (SetE v x) = "#[set " ++ show v ++ show x ++ "]"
  show c@(ConsE _ _) = "(" ++ showCons c
    where showCons (ConsE x NilE) = show x ++ ")"
          showCons (ConsE x xs)   = show x ++ " " ++ show xs ++ ")"
  show (LambdaE s e) = "#[lambda " ++ s ++ " " ++ show e ++ "]"
  show (QuotedE e) = show e
  show NilE     = "nil"

cons2List :: Expr -> [Expr]
cons2List NilE = []
cons2List (ConsE x xs) = x:cons2List xs

-- TODO: Make set! and lambda(?) parsed as cons, detect later set! and lambda as special procedures
