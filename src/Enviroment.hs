module Enviroment where

import qualified Data.Map as M
import Expression
import Data.Maybe

data Enviroment = Enviroment (M.Map String Expr) (Maybe Enviroment)

-- The base enviroment, that contains the main functions and variables
base :: Enviroment
base = Enviroment (M.fromList [
  ("id", LambdaE "x" $ VarE "x"),                 -- Given a expression, returns the same expression
  ("const", LambdaE "x" $ LambdaE "y" $ VarE "x") -- Given two expressions, returns the first expression
  ]) Nothing

lookupVar :: Enviroment -> String -> Maybe Expr
lookupVar (Enviroment menv upperEnv) var = let mExpr = M.lookup var menv in
  if isNothing mExpr then upperEnv >>= (`lookupVar` var) else mExpr

insertVar :: Enviroment -> String -> Expr -> Enviroment
insertVar (Enviroment menv u) var expr = Enviroment (M.insert var expr menv) u

emptyEnv :: Enviroment
emptyEnv = Enviroment M.empty Nothing

-- Returns a copy of the first enviroment whose upper enviroment is the second
extendEnv :: Enviroment -> Enviroment -> Enviroment
extendEnv (Enviroment menv Nothing) upper = Enviroment menv (Just upper)
extendEnv (Enviroment menv (Just upperMenv)) upper = Enviroment menv (Just $ extendEnv upperMenv upper)
