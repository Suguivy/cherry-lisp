module BuiltinProcs where

import Types.Language
import Control.Monad.State

car :: [Expr] -> State Enviroment Expr
car [ConsE x _] = return $ QuotedE x

cdr :: [Expr] -> State Enviroment Expr
cdr [ConsE _ xs] = return $ QuotedE xs

cons :: [Expr] -> State Enviroment Expr
cons [e1, e2] = return $ QuotedE $ ConsE e1 e2
