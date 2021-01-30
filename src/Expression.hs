module Expression where

data Expr = IntE Integer
          | VarE String
          | SetE String Expr
          | ConsE Expr Expr
          | LambdaE String Expr
          | QuotedE Expr
          | NilE

-- TODO: Make set! and lambda(?) parsed as cons, detect later set! and lambda as special procedures

instance Show Expr where
  show (IntE x) = show x
  show (VarE x) = x
  show (SetE v x) = "#[set " ++ show v ++ show x ++ "]"
  show c@(ConsE _ _) = "(" ++ showCons c
    where showCons (ConsE x NilE) = show x ++ ")"
          showCons (ConsE x xs)   = show x ++ " " ++ showCons xs
  show (LambdaE s e) = "#[lambda " ++ s ++ " " ++ show e ++ "]"
  show (QuotedE e) = show e
  show NilE     = "nil"
