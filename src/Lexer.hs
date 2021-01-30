module Lexer where

import Text.ParserCombinators.Parsec
import Data.Char
import TokenType

parseTokens :: GenParser Char st [Token]
parseTokens = do
    _ <-  spaces
    tokns <- many $ do t <- anyLispToken; spaces; return t
    _ <- eof
    return tokns

anyLispToken :: GenParser Char st Token
anyLispToken = leftParenT <|> rightParenT <|> apostropheT <|> symbolT <|> intT

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

symbolT :: GenParser Char st Token
symbolT = do
    var <- map toLower <$> many1 (letter <|> oneOf "+-*/\\!|@#$~%&/=<>")
    return $ SymbolT var
