module Enviroment where

import qualified Data.Map as M
import ExprType
import Data.Maybe

data Enviroment = Enviroment (M.Map String Expr) (Maybe Enviroment)

base :: Enviroment
base = Enviroment (M.fromList [
  ("test-var", IntE 10),
  ("id", LambdaE "x" $ VarE "x"),
  ("first", LambdaE "x" $ LambdaE "y" $ VarE "x")
  ]) Nothing

lookupVar :: Enviroment -> String -> Maybe Expr
lookupVar (Enviroment menv upperEnv) var = let mExpr = M.lookup var menv in
  if isNothing mExpr then upperEnv >>= (`lookupVar` var) else mExpr

insertVar :: Enviroment -> String -> Expr -> Enviroment
insertVar (Enviroment menv u) var expr = Enviroment (M.insert var expr menv) u

emptyEnv :: Enviroment
emptyEnv = Enviroment M.empty Nothing

extendEnv :: Enviroment -> Enviroment -> Enviroment
extendEnv (Enviroment menv Nothing) upper = Enviroment menv (Just upper)
extendEnv (Enviroment menv (Just upperMenv)) upper = Enviroment menv (Just $ extendEnv upperMenv upper)
