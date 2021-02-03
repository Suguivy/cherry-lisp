module Parser where

import Text.Parsec hiding (tokens)
import Text.Parsec.String
import Lexer (tokens)
import Token
import Types.Language

parseExpression :: String -> Either ParseError Expr
parseExpression s = do
  tokns <- parse tokens "lexical error" s
  expr <- parse anyExpressionEOF "parsing error" tokns
  return expr

anyExpressionEOF :: GenParser Token st Expr
anyExpressionEOF = do
  expr <- anyExpression
  _ <- eof
  return expr

anyExpression :: GenParser Token st Expr
anyExpression = do
  expr <- intE <|> quotedE <|> try setE <|> try nilE <|> try consE <|> try lambdaE <|> varE <|> listE
  return expr

------------------------------------------------------------

listE :: GenParser Token st Expr
listE = do
  _ <- parseLeftParenT
  exprs <- many anyExpression
  _ <- parseRightParenT
  return $ toCons exprs
  where toCons []     = NilE
        toCons (x:xs) = ConsE x (toCons xs)

intE :: GenParser Token st Expr
intE = do
    (IntT num) <- parseIntT
    return $ IntE num

quotedE :: GenParser Token st Expr
quotedE = do
  _ <- parseApostropheT
  expr <- anyExpression
  return $ QuotedE expr

consE :: GenParser Token st Expr
consE = do
  _ <- parseLeftParenT
  _ <- parseConsT
  expr1 <- anyExpression
  expr2 <- anyExpression
  _ <- parseRightParenT
  return $ ConsE expr1 expr2

varE :: GenParser Token st Expr
varE = do
  (SymbolT var) <- parseSymbolT
  return $ VarE var

nilE :: GenParser Token st Expr
nilE = do
  _ <- parseNilT
  return NilE

setE :: GenParser Token st Expr
setE = do
  _ <- parseLeftParenT
  _ <- parseSetT
  (SymbolT var) <- parseSymbolT
  expr <- anyExpression
  _ <- parseRightParenT
  return $ SetE var expr

lambdaE :: GenParser Token st Expr
lambdaE = do
  _ <- parseBackslashT
  (SymbolT arg) <- parseSymbolT
  body <- anyExpression
  return $ LambdaE arg body

------------------------------------------------------------

satisfyT :: (Stream s m a, Show a) => (a -> Bool) -> ParsecT s u m a
satisfyT f = tokenPrim show
                       (\pos _ _ -> incSourceColumn pos 1)
                       (\t -> if f t then Just t else Nothing)

parseBackslashT :: GenParser Token st Token
parseBackslashT = satisfyT (== BackslashT)

parseLeftParenT :: GenParser Token st Token
parseLeftParenT = satisfyT (== LeftParenT)

parseRightParenT :: GenParser Token st Token
parseRightParenT = satisfyT (== RightParenT)

parseApostropheT :: GenParser Token st Token
parseApostropheT = satisfyT (== ApostropheT)

parseSetT :: GenParser Token st Token
parseSetT = satisfyT isSetT
  where isSetT (SymbolT "set!") = True
        isSetT _               = False

parseNilT :: GenParser Token st Token
parseNilT = satisfyT isNilT
  where isNilT (SymbolT "nil") = True
        isNilT _               = False

parseConsT :: GenParser Token st Token
parseConsT = satisfyT isConsT
  where isConsT (SymbolT "cons") = True
        isConsT _               = False

parseSymbolT :: GenParser Token st Token
parseSymbolT = satisfyT isSymbolT
  where isSymbolT (SymbolT _) = True
        isSymbolT _        = False

parseIntT :: GenParser Token st Token
parseIntT = satisfyT isIntT
  where isIntT (IntT _) = True
        isIntT _        = False
