module Parser.Declaration (
  enumDeclP, 
  recDeclP,
  valDeclP
) where

import Text.Megaparsec
import Text.Megaparsec.Char

import Parser.Common
import Parser.Type (typeP)
import qualified AST.Untyped as U

enumDeclP :: Parser U.EnumDecl
enumDeclP = label "enum declaration" $ do
  offset <- getOffset
  string "enum"
  some spaceP
  declName <- nameP
  eol
  constructors <- some enumConstructorP
  (fmap (\_ -> ()) $ some eol) <|> eof
  return $ U.EnumDecl offset declName constructors


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


recDeclP :: Parser U.RecDecl
recDeclP = label "record declaration" $ do
  offset <- getOffset
  string "record"
  some spaceP
  declName <- nameP
  eol
  fields <- some recFieldP
  eol
  return $ U.RecDecl offset declName fields


recFieldP :: Parser U.RecField
recFieldP = do
  some spaceP
  offset <- getOffset
  fieldName <- nameP
  many spaceP 
  char ':'
  many spaceP
  fieldType <- typeP
  eol
  return $ U.RecField offset fieldName fieldType


valDeclP :: Parser U.ValDecl
valDeclP = label "value declaration" $ do
  offset <- getOffset
  declName <- nameP <|> operatorNameP
  many spaceP
  char ':'
  many spaceP
  declaredType <- typeP
  some eol
  return $ U.ValDecl offset declName declaredType
