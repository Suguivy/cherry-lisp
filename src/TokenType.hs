module TokenType where

data Token = LeftParenT
           | RightParenT
           | BackslashT
           | SymbolT String
           | ApostropheT
           | IntT Integer
           deriving (Show, Eq)
