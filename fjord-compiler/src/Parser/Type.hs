module Parser.Type (
  typeP,
  typeTermP
) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Control.Monad.Combinators.Expr as Expr

import AST.Common (Type (..))
import Parser.Common
import qualified AST.Common as Common
import qualified AST.Untyped as U

typeTermP :: [String] -> Parser Type
typeTermP typeVars = 
  choice [try emptyTupleP, try $ tupleTypeP typeVars, parenTypeP typeVars, typeNameP typeVars]


emptyTupleP :: Parser Type
emptyTupleP = 
  label "empty tuple" $ do
    offset <- getOffset
    char '('
    many spaceP
    char ')'
    return $ TupleType offset []


tupleTypeP :: [String] -> Parser Type
tupleTypeP typeVars = label "tuple type" $
  let 
    rhsP = do
      many spaceP
      char ','
      many spaceP
      expr <- typeP typeVars
      return expr
  in do
    offset <- getOffset
    char '('
    many spaceP
    head <- typeP typeVars
    tail <- some rhsP
    many spaceP
    char ')'
    return $ TupleType offset (head : tail)


parenTypeP :: [String] -> Parser Type
parenTypeP typeVars = do
  char '('
  many spaceP
  innerType <- typeP typeVars
  many spaceP
  char ')'
  return innerType


typeNameP :: [String] -> Parser Type
typeNameP typeVars = 
  label "type name" $
    do
      offset <- getOffset
      name <- nameP
      let nameType = if elem name typeVars then Common.TypeVar else Common.TypeRef
      return $ TypeName offset name nameType

typeP :: [String] -> Parser Type
typeP typeVars = 
  let 
    pureFunction = Expr.InfixR $ try $ label "function" $ do 
      offset <- getOffset
      many spaceP
      string "->"
      many spaceP
      return $ FunctionType offset
  in 
    Expr.makeExprParser (typeTermP typeVars) [
      [Expr.InfixL $ try $ typeApplyP], 
      [pureFunction]
    ]


typeApplyP = 
  label "type application" $ 
    do
      offset <- getOffset
      some spaceP
      notFollowedBy $ choice 
        [
          fmap (const ()) $ choice [string "->", string "-*", string "=>"],
          fmap (const ()) $ oneOf ":",
          fmap (const ()) $ eol
        ]
      return $ TypeApply offset
      