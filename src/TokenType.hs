module TokenType where

data Token = LeftParenT
           | RightParenT
           | SetT
           | VarT String
           | IntT Integer
           | NilT
           deriving (Show, Eq)
