{-# LANGUAGE FlexibleInstances #-}
module Parser (runModuleParser) where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import qualified AST.Contextual as C

type Parser = Parsec String String

runModuleParser :: String -> String -> Either (ParseErrorBundle String String) C.Module
runModuleParser = runParser moduleP

moduleP :: Parser C.Module
moduleP = do
  string "module"
  some spaceP
  moduleName <- nameP
  some eol
  declarations <- many declarationP 
  return $ C.Module moduleName declarations

declarationP :: Parser C.Declaration
declarationP = do
  offset <- getOffset
  declarationName <- nameP
  many spaceP
  char ':'
  many spaceP
  declaredType <- typeP
  some eol
  string declarationName
  many spaceP
  parameters <- many parameterP
  many spaceP
  char '='
  many spaceP
  value <- expressionP
  return $ C.ValueDeclaration offset declarationName parameters declaredType value

parameterP :: Parser C.Parameter
parameterP = do
  offset <- getOffset
  name <- nameP
  many spaceP
  return $ C.Parameter offset name

typeNameP :: Parser C.Type
typeNameP = do
  offset <- getOffset
  name <- nameP
  return $ C.Named offset name

functionTypeP :: Parser C.Type
functionTypeP = do
  offset <- getOffset
  parameterType <- typeP
  some spaceP
  string "->"
  some spaceP
  returnType <- typeP
  return $ C.FunctionType offset parameterType returnType

typeP :: Parser C.Type
typeP = 
  let 
    thinArrow = InfixR $ do 
      offset <- getOffset
      some spaceP
      (string "->")
      some spaceP
      return (C.FunctionType offset)
  in 
    makeExprParser typeNameP [[thinArrow]]

spaceP :: Parser Char
spaceP = oneOf " \t"

nameP :: Parser String
nameP = some letterChar

expressionP :: Parser C.Expression
expressionP = intLiteralP <|> stringLiteralP <|> nameExpressionP

intLiteralP :: Parser C.Expression
intLiteralP = do
  offset <- getOffset
  num <- decimal
  return $ C.IntLiteral offset num

stringLiteralP :: Parser C.Expression
stringLiteralP = do
  offset <- getOffset
  char '"'
  strings <- many stringPartP 
  char '"'
  return $ C.StringLiteral offset (concat strings)

nameExpressionP :: Parser C.Expression
nameExpressionP = do
  offset <- getOffset
  name <- nameP
  return $ C.Name offset name

nonEscape :: Parser String
nonEscape = do
  d <- char '\\'
  c <- oneOf "\\\"0nrvtbf"
  return [d, c]

escape :: Parser String
escape = some $ noneOf "\\\"\0\n\r\v\t\b\f"

stringPartP :: Parser String
stringPartP = nonEscape <|> escape

instance ShowErrorComponent String where
  showErrorComponent = id
  errorComponentLen = length
