{-# LANGUAGE FlexibleInstances #-}
module Parser (runModuleParser) where

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
  declaredType <- typeNameP
  some eol
  string declarationName
  many spaceP
  char '='
  many spaceP
  value <- intLiteralP <|> stringLiteralP
  return $ C.ValueDeclaration offset declarationName declaredType value

typeNameP :: Parser C.Type
typeNameP = do
  offset <- getOffset
  name <- (string "Int") <|> (string "String")
  return $ C.Named offset name

spaceP :: Parser Char
spaceP = oneOf " \t"

nameP :: Parser String
nameP = some letterChar

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
