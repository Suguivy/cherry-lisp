module Enviroment where

import qualified Data.Map as M
import Types.Language
import Data.Maybe

-- The base enviroment, that contains the main functions and variables
base :: Enviroment
base = Enviroment (M.fromList [
  ("id", LambdaE "x" $ VarE "x"),                 -- Given a expression, returns the same expression
  ("const", LambdaE "x" $ LambdaE "y" $ VarE "x") -- Given two expressions, returns the first expression
  ]) Nothing

lookupVar :: String -> Enviroment -> Maybe Expr
lookupVar var (Enviroment menv upperEnv) = let mExpr = M.lookup var menv in
  if isNothing mExpr then upperEnv >>= lookupVar var else mExpr

insertVar :: String -> Expr -> Enviroment -> Enviroment
insertVar var expr (Enviroment menv u) = Enviroment (M.insert var expr menv) u

emptyEnv :: Enviroment
emptyEnv = Enviroment M.empty Nothing

-- Returns a copy of the second enviroment whose upper enviroment is the first
extendEnv :: Enviroment -> Enviroment -> Enviroment
extendEnv upper (Enviroment menv Nothing) = Enviroment menv (Just upper)
extendEnv upper (Enviroment menv (Just upperMenv)) = Enviroment menv (Just $ extendEnv upperMenv upper)
