module ExprType where

data Expr = IntE Integer
          | VarE String
          | ProcedureE String [Expr]
          | SetE String Expr
          | NilE deriving (Show)

-- instance Show Expr where
--   show (IntE x) = show x
--   show (VarE x) = x ++ " ; var"
--   show NilE     = "nil"
