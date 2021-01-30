module ExprType where

data Expr = IntE Integer
          | VarE String
          | SetE String Expr
          | ConsE Expr Expr
          | LambdaE String Expr
          | QuotedE Expr
          | NilE
          deriving (Show)

-- instance Show Expr where
--   show (IntE x) = show x
--   show (VarE x) = x ++ " ; var"
--   show NilE     = "nil"
