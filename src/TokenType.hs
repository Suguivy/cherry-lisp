module TokenType where

data Token = LeftParenT
           | RightParenT
           | SymbolT String
           | IntT Integer
           deriving (Show, Eq)
