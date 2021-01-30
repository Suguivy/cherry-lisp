module TokenType where

data Token = LeftParenT
           | RightParenT
           | SymbolT String
           | ApostropheT
           | IntT Integer
           deriving (Show, Eq)
