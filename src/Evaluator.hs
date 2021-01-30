module Evaluator where

import Expression
import Enviroment
import Data.Maybe

-- TODO: create a separated file for builtinProcs
-- TODO: create a BuiltinProc or something like that in data Expr, and make + a builtin proc

eval :: Enviroment -> Expr -> (Enviroment, Expr)
eval env i@(IntE _)     = (env, i)
eval env (VarE v)       = eval env $ fromJust $ lookupVar env v
eval env (SetE v expr)  = (insertVar env v expr, NilE)
eval env (QuotedE e)    = (env, e)
eval env (ConsE p args) = (fEnv, fExpr)
  where (uEnv, ap)    = eval env p
        (aEnv, aExpr) = apply ap args
        (fEnv, fExpr) = eval (extendEnv aEnv uEnv) aExpr
eval env (LambdaE arg expr) = (env, LambdaE arg expr)
eval env NilE          = (env, NilE)

apply :: Expr -> Expr -> (Enviroment, Expr)
apply (LambdaE p expr) (ConsE x xs) = (extendEnv (insertVar emptyEnv p x) nEnv, nExp)
  where (nEnv, nExp) = apply expr xs
apply e NilE = (emptyEnv, e)
