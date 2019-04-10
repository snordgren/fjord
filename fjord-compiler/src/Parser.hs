{-# LANGUAGE FlexibleInstances #-}
module Parser (runModuleParser, expressionP) where

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
  moduleName <- qualifiedNameP
  many spaceP
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
  some eol
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
spaceP = oneOf " \t\r"

nameP :: Parser String
nameP = some letterChar

qualifiedNameP :: Parser String
qualifiedNameP = do
  head <- some letterChar
  tail <- many (letterChar <|> (char '.'))
  return (head ++ tail)

termP :: Parser C.Expression
termP = intLiteralP <|> stringLiteralP <|> nameExpressionP

applyP = do
  offset <- getOffset
  some spaceP
  notFollowedBy additionP
  return $ C.Apply offset

additionP = do
  offset <- getOffset
  many spaceP
  char '+'
  many spaceP
  return $ C.Addition offset

expressionP = makeExprParser termP [[InfixL (try applyP)], [InfixL (try additionP)]]

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
