module TokenParser where

import Text.ParserCombinators.Parsec
import ParserUtils
import Data.Char
import TokenType

parseTokens :: GenParser Char st [Token]
parseTokens = do
    _ <-  spaces
    tokns <- many $ do t <- anyLispToken; spaces; return t
    _ <- eof
    return tokns

anyLispToken :: GenParser Char st Token
anyLispToken = leftParenT <|> rightParenT <|> try nilT <|> try defineT <|> varT <|> intT

------------------------------------------------------------

intT :: GenParser Char st Token
intT = do
    number <- read <$> many1 digit
    return $ IntT number

leftParenT :: GenParser Char st Token
leftParenT = char '(' >> return LeftParenT

rightParenT :: GenParser Char st Token
rightParenT = char ')' >> return RightParenT

defineT :: GenParser Char st Token
defineT = caseInsensitiveString "define" >> return DefineT

nilT :: GenParser Char st Token
nilT = caseInsensitiveString "nil" >> return NilT

varT :: GenParser Char st Token
varT = do
    var <- map toLower <$> many1 (letter <|> oneOf "+-*/!|@#$~%&/=<>")
    return $ VarT var
