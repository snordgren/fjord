module Parser.Declaration (
  enumDeclP, 
  implicitDeclP,
  recDeclP,
  valDeclP
) where

import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char

import Parser.Common
import Parser.Type (typeP, typeTermP)
import qualified AST.Untyped as U

enumDeclP :: Parser U.EnumDecl
enumDeclP = label "enum declaration" $ do
  offset <- getOffset
  string "enum"
  some spaceP
  declName <- nameP
  typeVars <- many (try $ (some spaceP) >> nameP)
  many spaceP
  eol
  constructors <- some enumConstructorP
  (fmap (\_ -> ()) $ some eol) <|> eof
  return $ U.EnumDecl offset declName constructors typeVars


enumConstructorP :: Parser U.EnumConstructor
enumConstructorP = label "enum constructor" $ do
  some spaceP
  offset <- getOffset
  constructorName <- nameP
  parTypes <- many $ try $ some spaceP >> typeP
  many spaceP
  char ':'
  many spaceP
  retType <- typeP
  many spaceP
  eol
  return $ U.EnumConstructor offset constructorName parTypes retType


implicitDeclP :: Parser U.ValDecl 
implicitDeclP = do
  string "implicit"
  many spaceP
  some eol
  offset <- getOffset
  declName <- nameP
  many spaceP
  char ':'
  many spaceP
  declaredType <- typeP
  many spaceP
  some eol
  return $ U.ValDecl offset declName declaredType


recDeclP :: Parser U.RecDecl
recDeclP = label "record declaration" $ do
  offset <- getOffset
  string "record"
  some spaceP
  declName <- nameP
  typeVars <- many (try $ (some spaceP) >> nameP)
  many spaceP
  eol
  fields <- many $ try $ recFieldP
  (eol >> return ()) <|> (eof >> return ())
  return $ U.RecDecl offset declName fields typeVars


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
  many spaceP
  declaredType <- typeP
  many spaceP
  some eol
  return $ U.ValDecl offset declName declaredType
