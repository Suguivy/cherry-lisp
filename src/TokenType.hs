module TokenType where

data Token = LeftParenT
           | RightParenT
           | DefineT
           | VarT String
           | IntT Integer
           | NilT
           deriving (Show, Eq)
