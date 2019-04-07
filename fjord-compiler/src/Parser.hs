{-# LANGUAGE FlexibleInstances #-}
module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import AST.Contextual

type Parser = Parsec String String

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
  string "Int" <|> string "String"
  some eol
  string declarationName
  many spaceP
  char '='
  many spaceP
  value <- (fmap IntLiteral decimal) <|> stringP
  return $ ValueDeclaration declarationName value

spaceP :: Parser Char
spaceP = oneOf " \t"

nameP :: Parser String
nameP = some letterChar

stringP :: Parser Expression
stringP = do
  char '"'
  strings <- many stringPartP 
  char '"'
  return $ StringLiteral $ concat strings

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
