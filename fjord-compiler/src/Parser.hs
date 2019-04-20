{-# LANGUAGE FlexibleInstances #-}
module Parser (runModuleParser, expressionP) where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import qualified Data.List as List

import Parsing.Basics
import Parsing.Expression
import qualified AST.Untyped as U


runModuleParser :: String -> String -> Either (ParseErrorBundle String String) U.Module
runModuleParser = 
  runParser moduleP

  
moduleP :: Parser U.Module
moduleP = do
  string "module"
  some spaceP
  moduleName <- qualifiedNameP
  many spaceP
  some eol
  declarations <- some declarationP 
  return $ U.Module moduleName declarations


declarationP :: Parser U.Declaration
declarationP = 
  label "declaration" $ enumDeclarationP <|> recordDeclarationP <|> valueDeclarationP


enumDeclarationP :: Parser U.Declaration
enumDeclarationP = label "enum declaration" $ do
  offset <- getOffset
  string "enum"
  some spaceP
  declarationName <- nameP
  eol
  constructors <- some enumConstructorP
  (fmap (\_ -> ()) $ some eol) <|> eof
  return $ U.EnumDeclaration offset declarationName constructors


enumConstructorP :: Parser U.EnumConstructor
enumConstructorP = label "enum constructor" $ do
  some spaceP
  offset <- getOffset
  constructorName <- nameP
  many spaceP
  char ':'
  many spaceP
  t <- typeP
  eol
  return $ U.EnumConstructor offset constructorName t 


recordDeclarationP :: Parser U.Declaration
recordDeclarationP = label "record declaration" $ do
  offset <- getOffset
  string "record"
  some spaceP
  declarationName <- nameP
  eol
  fields <- some recordFieldP
  eol
  return $ U.RecordDeclaration offset declarationName fields


recordFieldP :: Parser U.RecordField
recordFieldP = do
  some spaceP
  offset <- getOffset
  fieldName <- nameP
  many spaceP 
  char ':'
  many spaceP
  fieldType <- typeP
  eol
  return $ U.RecordField offset fieldName fieldType


valueDeclarationP :: Parser U.Declaration
valueDeclarationP = label "value declaration" $ do
  offset <- getOffset
  declarationName <- nameP <|> operatorNameP
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
  let useName = filter (\a -> a /= '(' && a /= ')') declarationName
  return $ U.ValueDeclaration offset useName parameters declaredType value

parameterP :: Parser U.Parameter
parameterP = do
  offset <- getOffset
  name <- nameP
  many spaceP
  return $ U.Parameter offset name


typeTermP :: Parser U.Type
typeTermP = 
  choice [(try tupleTypeP), parenTypeP, typeNameP]


tupleTypeP :: Parser U.Type
tupleTypeP = label "tuple type" $
  let 
    rhsP = do
      many spaceP
      char ','
      many spaceP
      expr <- typeP
      return expr
  in do
    offset <- getOffset
    char '('
    many spaceP
    head <- typeP
    tail <- some rhsP
    many spaceP
    char ')'
    return $ U.TupleType offset $ head : tail


parenTypeP :: Parser U.Type
parenTypeP = do
  char '('
  many spaceP
  innerType <- typeP
  many spaceP
  char ')'
  return innerType


typeNameP :: Parser U.Type
typeNameP = do
  offset <- getOffset
  name <- nameP
  return $ U.TypeName offset name

typeP :: Parser U.Type
typeP = 
  let 
    thinArrow = InfixR $ do 
      offset <- getOffset
      some spaceP
      (string "->")
      some spaceP
      return (U.FunctionType offset)
  in 
    makeExprParser typeTermP [[thinArrow]]


instance ShowErrorComponent String where
  showErrorComponent = id
  errorComponentLen = length
