module Evaluator where

import ExprType
import Enviroment
import Data.Maybe

-- TODO: create a separated file for builtinProcs
-- TODO: create a BuiltinProc or something like that in data Expr, and make + a builtin proc

eval :: Enviroment -> Expr -> (Enviroment, Expr)
eval env i@(IntE _)    = (env, i)
eval env (VarE v)      = eval env $ fromJust $ lookupVar env v
eval env (SetE v expr) = (insertVar env v expr, NilE)
eval env (QuotedE e)    = (env, e)
eval env (ConsE car cdr) = (env, apply car cdr)
eval env NilE          = (env, NilE)

apply :: Expr -> Expr -> Expr
apply car cdr = undefined
