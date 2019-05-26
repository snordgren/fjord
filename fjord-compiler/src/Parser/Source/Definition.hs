module Parser.Source.Definition (defP) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Parser.Common
import Parser.Type (typeP)
import Parser.Declaration (enumDeclP, implicitDeclP, recDeclP, valDeclP)
import Parser.Source.Expression (expressionP)
import qualified AST.Untyped as U

defP :: Parser U.Definition
defP = 
  label "definition" $ choice
    [
      enumDefP,
      implicitDefP,
      recDefP,
      valDefP
    ]


enumDefP :: Parser U.Definition
enumDefP = 
  label "enum definition" $ do
    e <- enumDeclP
    return $ U.EnumDef e


implicitDefP :: Parser U.Definition
implicitDefP =
  label "implicit definition" $ do
    v <- implicitDeclP
    string $ U.valDeclName v
    many spaceP
    char '='
    spaceInExpressionP
    value <- expressionP
    some eol
    return $ U.ImplicitDef v value


recDefP :: Parser U.Definition
recDefP =
  label "record definition" $ do
    r <- recDeclP
    return $ U.RecDef r

    
valDefP :: Parser U.Definition
valDefP = 
  label "value definition" $ do
    v <- valDeclP
    string $ U.valDeclName v
    many spaceP
    params <- many parameterP
    many spaceP
    char '='
    spaceInExpressionP
    value <- expressionP
    many eol
    return $ U.ValDef v params value
    

parameterP :: Parser U.Parameter
parameterP = do
  offset <- getOffset
  name <- nameP
  many spaceP
  return $ U.Parameter offset name
