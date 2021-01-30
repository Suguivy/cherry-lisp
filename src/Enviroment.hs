module Enviroment where

import qualified Data.Map as M
import ExprType
import Data.Maybe

data Enviroment = Enviroment (M.Map String Expr) (Maybe Enviroment)

base :: Enviroment
base = Enviroment (M.fromList [
  ("test-var", IntE 10)
  ]) Nothing

lookupVar :: Enviroment -> String -> Maybe Expr
lookupVar (Enviroment menv upperEnv) var = let mExpr = M.lookup var menv in
  if isNothing mExpr then upperEnv >>= (`lookupVar` var) else mExpr

insertVar :: Enviroment -> String -> Expr -> Enviroment
insertVar (Enviroment menv u) var expr = Enviroment (M.insert var expr menv) u
