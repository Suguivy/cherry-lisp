-- UNUSED
module ParserUtils where

import Text.ParserCombinators.Parsec
import Data.Char

caseInsensitiveString :: String -> GenParser Char st String
caseInsensitiveString ""     = return ""
caseInsensitiveString [x]    = (: []) <$> caseInsensitiveChar x
caseInsensitiveString (x:xs) = do
    c <- caseInsensitiveChar x
    s <- caseInsensitiveString xs
    return (c:s)

caseInsensitiveChar :: Char -> GenParser Char st Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)
