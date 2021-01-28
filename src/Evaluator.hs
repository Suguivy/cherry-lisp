module Evaluator where

import qualified Data.Map as M
import ExprType

type Env = M.Map String Expr
type Proc = String
type Args = [Expr]

eval :: Expr -> Expr
eval (IntE x)             = IntE x
eval (VarE v)             = VarE v
eval (DefinitionE v exp)  = NilE
eval (ProcedureE p args)  = apply p args
eval NilE                 = NilE

apply :: Proc -> Args -> Expr
apply p args
    | p == "+" = builtinPlus $ map eval args
-- TODO: create a separated file for builtinProcs
-- TODO: create a BuiltinProc or something like that in data Expr, and make + a builtin proc
-- TODO: create enviroments

builtinPlus :: [Expr] -> Expr
builtinPlus [IntE x, IntE y] = IntE $ x + y
builtinPlus ((IntE x):xs) = IntE $ x + n
  where (IntE n) = builtinPlus xs
