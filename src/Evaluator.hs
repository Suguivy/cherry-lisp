module Evaluator where

import qualified Data.Map as M
import ExprType

type Env = M.Map String Expr

-- TODO: create a separated file for builtinProcs
-- TODO: create a BuiltinProc or something like that in data Expr, and make + a builtin proc

base :: Env
base = M.fromList [
  ("test-var", IntE 10)
  ]

eval :: Env -> Expr -> (Env, Expr)
eval env i@(IntE _)    = (env, i)
eval env (VarE v)      = eval env $ env M.! v
eval env (SetE v expr) = (M.insert v expr env, NilE)
eval env (QuotedE e)    = (env, e)
eval env (ConsE car cdr) = (env, apply car cdr)
eval env NilE          = (env, NilE)

apply :: Expr -> Expr -> Expr
apply car cdr = undefined
