module Enviroment.Base where

import BuiltinProcs
import Types.Language
import Data.Map as M

base :: Enviroment
base = Enviroment (M.fromList [
  ("id", LambdaE "x" $ VarE "x"),                  -- Given a expression, returns the same expression
  ("const", LambdaE "x" $ LambdaE "y" $ VarE "x"), -- Given two expressions, returns the first expression
  ("car", BuiltinProcE car),
  ("cdr", BuiltinProcE cdr),
  ("cons", BuiltinProcE cons)
  ]) Nothing
