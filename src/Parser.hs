module Parser where

import Text.Parsec
import Text.Parsec.String
import Lexer
import TokenType
import ExprType

parseExpression :: String -> Either ParseError Expr
parseExpression s = do
  tokns <- parse parseTokens "lexical error" s
  expr <- parse expressionFromTokensEOF "parsing error" tokns
  return expr

expressionFromTokensEOF :: GenParser Token st Expr
expressionFromTokensEOF = do
  expr <- expressionFromTokens
  _ <- eof
  return expr

expressionFromTokens :: GenParser Token st Expr
expressionFromTokens = do
  expr <- intE <|> quotedE <|> try setE <|> try nilE <|> try consE <|> varE <|> procedureE
  return expr

intE :: GenParser Token st Expr
intE = do
    (IntT num) <- parseIntT
    return $ IntE num

quotedE :: GenParser Token st Expr
quotedE = do
  _ <- parseApostropheT
  expr <- expressionFromTokens
  return $ QuotedE expr

procedureE :: GenParser Token st Expr
procedureE = do
  _ <- parseLeftParenT
  (SymbolT p) <- parseSymbolT
  args <- many expressionFromTokens
  _ <- parseRightParenT
  return $ ProcedureE p args

consE :: GenParser Token st Expr
consE = do
  _ <- parseLeftParenT
  _ <- parseConsT
  expr1 <- expressionFromTokens
  expr2 <- expressionFromTokens
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
  expr <- expressionFromTokens
  _ <- parseRightParenT
  return $ SetE var expr

------------------------------------------------------------

satisfyT :: (Stream s m a, Show a) => (a -> Bool) -> ParsecT s u m a
satisfyT f = tokenPrim show
                       (\pos _ _ -> incSourceColumn pos 1)
                       (\t -> if f t then Just t else Nothing)

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
