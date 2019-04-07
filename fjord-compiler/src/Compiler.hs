module Compiler ( 
  compile
) where

import Data.Either.Combinators
import Data.List (intercalate)
import Data.Void
import Text.Megaparsec

import AST.Contextual
import Parser (moduleP)

compile :: String -> String -> String
compile fileName source = 
  either (errorBundlePretty) (generateJSModule) (parseModuleSource fileName source)

parseModuleSource :: String -> String -> Either (ParseErrorBundle String String) Module
parseModuleSource fileName source = 
  (runParser moduleP fileName source)

generateJSModule :: Module -> String
generateJSModule m = 
  (intercalate "\n\n" (fmap translateDeclaration (moduleDeclarations m))) ++ "\n"

translateDeclaration :: Declaration -> String
translateDeclaration (ValueDeclaration name expression) =
  "export const " ++ name ++ " = " ++ (translateExpression expression) ++ ";"

translateExpression :: Expression -> String
translateExpression (IntLiteral a) = show a
translateExpression (StringLiteral a) = show a
