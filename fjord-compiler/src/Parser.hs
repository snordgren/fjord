{-# LANGUAGE FlexibleInstances #-}
module Parser (runModuleParser, expressionP) where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import qualified Data.List as List

import Parsing.Basics
import Parsing.Expression
import qualified AST.Contextual as C


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
  (fmap (\_ -> ()) $ some eol) <|> eof
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
  spaceInExpressionP
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


instance ShowErrorComponent String where
  showErrorComponent = id
  errorComponentLen = length
