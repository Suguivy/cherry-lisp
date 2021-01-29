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
eval env (IntE x)      = (env, IntE x)
eval env (VarE v)      = eval env $ env M.! v
eval env (SetE v expr) = (M.insert v expr env, expr)
eval env NilE          = (env, NilE)

--apply :: Proc -> Args -> Expr
--apply p args
