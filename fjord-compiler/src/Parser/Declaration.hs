module Parser.Declaration (
  enumDeclP, 
  implicitDeclP,
  recDeclP,
  valDeclP
) where

import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad (void)
import qualified Data.List as List

import AST.Common (Type (..))
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
  constructors <- some $ enumConstructorP typeVars
  (fmap (\_ -> ()) $ some eol) <|> eof
  return $ U.EnumDecl offset declName constructors typeVars


enumConstructorP :: [String] -> Parser U.EnumConstructor
enumConstructorP typeVars = label "enum constructor" $ do
  some spaceP
  offset <- getOffset
  constructorName <- nameP
  parTypes <- many $ try $ some spaceP >> (typeP typeVars)
  many spaceP
  char ':'
  many spaceP
  retType <- typeP typeVars
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
  (implicits, declaredType) <- valDeclTypeP
  many spaceP
  some eol
  return $ U.ValDecl offset declName declaredType implicits


recDeclP :: Parser U.RecDecl
recDeclP = label "record declaration" $ do
  offset <- getOffset
  string "record"
  some spaceP
  declName <- nameP
  typeVars <- many (try $ (some spaceP) >> nameP)
  many spaceP
  eol
  fields <- many $ try $ recFieldP typeVars
  (void eol) <|> (void eof)
  return $ U.RecDecl offset declName fields typeVars


recFieldP :: [String] -> Parser U.RecField
recFieldP typeVars = do
  some spaceP
  offset <- getOffset
  fieldName <- nameP
  many spaceP 
  char ':'
  many spaceP
  fieldType <- typeP typeVars
  eol
  return $ U.RecField offset fieldName fieldType


valDeclP :: Parser U.ValDecl
valDeclP = label "value declaration" $ do
  offset <- getOffset
  declName <- nameP <|> operatorNameP
  many spaceP
  char ':'
  many spaceP
  (implicits, declaredType) <- valDeclTypeP
  many spaceP
  some eol
  return $ U.ValDecl offset declName declaredType implicits


{- 
Parse a value type declaration. Returns a tuple containing first the implicits and
then the remaining type of the declaration.
-}
valDeclTypeP :: Parser ([Type], Type)
valDeclTypeP =
  let 
    typeVarP :: Parser String
    typeVarP = 
      label "type variable" $ do
        name <- nameP
        many spaceP
        string "."
        many spaceP
        return name

    implicitP :: [String] -> Parser Type
    implicitP typeVars = 
      label "implicit value" $ do
        t <- typeP typeVars
        many spaceP
        string "=>"
        many spaceP
        return t
  in
    do
      offset <- getOffset
      typeVars <- many $ try $ typeVarP
      implicits <- many $ try $ implicitP typeVars
      rest <- typeP typeVars
      return (implicits, List.foldl' (\b a -> TypeLambda offset a b) rest typeVars)
