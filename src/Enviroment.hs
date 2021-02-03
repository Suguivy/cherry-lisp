module Enviroment where

import qualified Data.Map as M
import Types.Language
import Data.Maybe

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
