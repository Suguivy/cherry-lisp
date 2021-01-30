module Lexer where

import Text.ParserCombinators.Parsec hiding (anyToken)
import Data.Char
import Token

tokens :: GenParser Char st [Token]
tokens = do
    _ <-  spaces
    tokns <- many $ do t <- anyToken; spaces; return t
    _ <- eof
    return tokns

anyToken :: GenParser Char st Token
anyToken = leftParenT <|> rightParenT <|> apostropheT <|> backslashT <|> symbolT <|> intT

------------------------------------------------------------

intT :: GenParser Char st Token
intT = do
    number <- read <$> many1 digit
    return $ IntT number

leftParenT :: GenParser Char st Token
leftParenT = char '(' >> return LeftParenT

apostropheT :: GenParser Char st Token
apostropheT = char '\'' >> return ApostropheT

rightParenT :: GenParser Char st Token
rightParenT = char ')' >> return RightParenT

backslashT :: GenParser Char st Token
backslashT = char '\\' >> return BackslashT

symbolT :: GenParser Char st Token
symbolT = do
    var <- map toLower <$> many1 (letter <|> oneOf "+-*/!|@#$~%&/=<>")
    return $ SymbolT var
