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

typeTermP :: Parser Type
typeTermP = 
  choice [try emptyTupleP, try tupleTypeP, parenTypeP, typeNameP]


emptyTupleP :: Parser Type
emptyTupleP = 
  label "empty tuple" $ do
    offset <- getOffset
    char '('
    many spaceP
    char ')'
    return $ TupleType offset []


tupleTypeP :: Parser Type
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
    return $ TupleType offset (head : tail)


parenTypeP :: Parser Type
parenTypeP = do
  char '('
  many spaceP
  innerType <- typeP
  many spaceP
  char ')'
  return innerType


typeNameP :: Parser Type
typeNameP = 
  label "type name" $
    do
      offset <- getOffset
      name <- nameP
      return $ TypeName offset name

typeP :: Parser Type
typeP = 
  let 
    pureFunction = Expr.InfixR $ try $ label "function" $ do 
      offset <- getOffset
      many spaceP
      string "->"
      many spaceP
      return $ FunctionType offset
  in 
    Expr.makeExprParser typeTermP [
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

-- Matches a type term or type application. 
implicitTypeP =
  Expr.makeExprParser typeTermP [
      [Expr.InfixL $ try $ typeApplyP]
  ]
     
