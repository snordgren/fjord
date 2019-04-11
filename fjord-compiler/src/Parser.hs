{-# LANGUAGE FlexibleInstances #-}
module Parser (runModuleParser, expressionP) where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import qualified AST.Contextual as C

type Parser = Parsec String String

runModuleParser :: String -> String -> Either (ParseErrorBundle String String) C.Module
runModuleParser = 
  runParser moduleP

  
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
declarationP = 
  enumDeclarationP <|> recordDeclarationP <|> valueDeclarationP


enumDeclarationP :: Parser C.Declaration
enumDeclarationP = do
  offset <- getOffset
  string "enum"
  some spaceP
  declarationName <- nameP
  eol
  constructors <- some enumConstructorP
  return $ C.EnumDeclaration offset declarationName constructors


enumConstructorP :: Parser C.EnumConstructor
enumConstructorP = do
  some spaceP
  offset <- getOffset
  constructorName <- nameP
  many spaceP
  char ':'
  many spaceP
  t <- typeP
  eol
  return $ C.EnumConstructor offset constructorName t 


recordDeclarationP :: Parser C.Declaration
recordDeclarationP = do
  offset <- getOffset
  string "record"
  some spaceP
  declarationName <- nameP
  eol
  fields <- some recordFieldP
  eol
  return $ C.RecordDeclaration offset declarationName fields


recordFieldP :: Parser C.RecordField
recordFieldP = do
  some spaceP
  offset <- getOffset
  fieldName <- nameP
  many spaceP 
  char ':'
  many spaceP
  fieldType <- typeP
  eol
  return $ C.RecordField offset fieldName fieldType


valueDeclarationP :: Parser C.Declaration
valueDeclarationP = do
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
termP = 
  intLiteralP <|> stringLiteralP <|> nameExpressionP <|> recordUpdateP


applyP = do
  offset <- getOffset
  some spaceP
  notFollowedBy $ choice 
    [
      fmap (\_ -> ()) additionP, 
      fmap (\_ -> ()) $ char '}'
    ]
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
  num <- some (oneOf "1234567890")
  return $ C.IntLiteral offset (read num :: Integer)


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


recordUpdateP :: Parser C.Expression
recordUpdateP = do
  offset <- getOffset
  char '{'
  many spaceP
  target <- nameExpressionP
  many spaceP
  char '|'
  many spaceP
  updateHead <- fieldUpdateP
  updateTail <- many ((char ',') >> (many spaceP) >> fieldUpdateP)
  many spaceP
  char '}'
  let updates = updateHead : updateTail
  return $ C.RecordUpdate offset target updates


fieldUpdateP :: Parser C.FieldUpdate
fieldUpdateP = do
  offset <- getOffset
  fieldName <- nameP
  many spaceP
  char '='
  many spaceP
  value <- expressionP
  return $ C.FieldUpdate offset fieldName value


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
