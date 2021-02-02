module Evaluator where

import Expression
import Enviroment
import Data.Maybe
import Control.Monad.State

-- TODO: create a separated file for builtinProcs
-- TODO: create a BuiltinProc or something like that in data Expr, and make + a builtin proc

evalS :: Expr -> State Enviroment Expr
evalS i@(IntE _) = return i
evalS (VarE v) = do
  get >>= evalS . fromJust . lookupVar v
evalS (SetE v expr) = do
  get >>= put . insertVar v expr
  return NilE
evalS (QuotedE e) = return e
evalS (ConsE pr args) = do
  evaluatedProc <- evalS pr
  resExpr <- applyS evaluatedProc args
  finalExpr <- evalS resExpr
  return finalExpr
evalS l@(LambdaE _ _) = return l
evalS NilE = return NilE

applyS :: Expr -> Expr -> State Enviroment Expr
applyS (LambdaE p expr) (ConsE x xs) = do
  e <- applyS expr xs
  get >>= put . (`extendEnv` insertVar p x emptyEnv)
  return e
applyS e NilE = return e
