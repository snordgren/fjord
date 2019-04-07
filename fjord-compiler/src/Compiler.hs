{-# LANGUAGE FlexibleInstances #-}
module Compiler ( 
  compile
) where

import Data.Either.Combinators
import Data.List (intercalate)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec String String

data Module = Module { moduleName :: String, moduleDeclarations :: [Declaration] }
type Type = String
data Expression = IntLiteral Integer
data Declaration = ValueDeclaration String Expression

compile :: String -> String -> String
compile fileName source = 
  either (errorBundlePretty) (generateJSModule) (parseModuleSource fileName source)

parseModuleSource :: String -> String -> Either (ParseErrorBundle String String) Module
parseModuleSource fileName source = 
  (runParser moduleP fileName source)

moduleP :: Parser Module
moduleP = do
  string "module"
  some spaceP
  moduleName <- nameP
  some eol
  declarations <- many declarationP 
  return $ Module moduleName declarations

declarationP :: Parser Declaration
declarationP = do
  declarationName <- nameP
  many spaceP
  char ':'
  many spaceP
  string "Int"
  some eol
  string declarationName
  many spaceP
  char '='
  many spaceP
  value <- decimal
  return $ ValueDeclaration declarationName (IntLiteral value)

spaceP :: Parser Char
spaceP = oneOf " \t"

nameP :: Parser String
nameP = many letterChar

generateJSModule :: Module -> String
generateJSModule m = 
  (intercalate "\n\n" (fmap translateDeclaration (moduleDeclarations m))) ++ "\n"

translateDeclaration :: Declaration -> String
translateDeclaration (ValueDeclaration name expression) =
  "export const " ++ name ++ " = " ++ (translateExpression expression) ++ ";"

translateExpression :: Expression -> String
translateExpression (IntLiteral a) = show a

instance ShowErrorComponent String where
  showErrorComponent = id
  errorComponentLen = length
