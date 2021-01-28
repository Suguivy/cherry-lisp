module ExprParser where

import Text.Parsec
import Text.Parsec.String
import TokenParser
import TokenType
import ExprType

parseExpression :: String -> Either ParseError Expr
parseExpression s = do
  tokns <- parse parseTokens "lexical error" s
  expr <- parse parseExpressionFromTokens "parsing error" tokns
  return expr

parseExpressionFromTokens :: GenParser Token st Expr
parseExpressionFromTokens = intE <|> try defineE <|> try nilE <|> varE <|> procedureE

intE :: GenParser Token st Expr
intE = do
    (IntT num) <- parseIntT
    return $ IntE num

procedureE :: GenParser Token st Expr
procedureE = do
  _ <- parseLeftParenT
  (VarT p) <- parseVarT
  args <- many parseExpressionFromTokens
  _ <- parseRightParenT
  return $ ProcedureE p args

varE :: GenParser Token st Expr
varE = do
  (VarT var) <- parseVarT
  return $ VarE var

nilE :: GenParser Token st Expr
nilE = do
  _ <- parseNilT
  return NilE

defineE :: GenParser Token st Expr
defineE = do
  _ <- parseLeftParenT
  _ <- parseDefineT
  (VarT var) <- parseVarT
  expr <- parseExpressionFromTokens
  _ <- parseRightParenT
  return $ DefinitionE var expr

------------------------------------------------------------

satisfyT :: (Stream s m a, Show a) => (a -> Bool) -> ParsecT s u m a
satisfyT f = tokenPrim show
                       (\pos _ _ -> incSourceColumn pos 1)
                       (\t -> if f t then Just t else Nothing)

parseLeftParenT :: GenParser Token st Token
parseLeftParenT = satisfyT (== LeftParenT)

parseRightParenT :: GenParser Token st Token
parseRightParenT = satisfyT (== RightParenT)

parseDefineT :: GenParser Token st Token
parseDefineT = satisfyT (== DefineT)

parseNilT :: GenParser Token st Token
parseNilT = satisfyT (== NilT)

parseVarT :: GenParser Token st Token
parseVarT = satisfyT isVarT
  where isVarT (VarT _) = True
        isVarT _        = False

parseIntT :: GenParser Token st Token
parseIntT = satisfyT isIntT
  where isIntT (IntT _) = True
        isIntT _        = False
