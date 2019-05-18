module Parser.Type (
  typeP,
  typeTermP
) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Control.Monad.Combinators.Expr as Expr

import Parser.Common
import qualified AST.Untyped as U

typeTermP :: Parser U.Type
typeTermP = 
  choice [try bindImplicitP, try emptyTupleP, try tupleTypeP, parenTypeP, typeNameP]


bindImplicitP :: Parser U.Type
bindImplicitP =
  label "implicit type binding" $ 
    do
      offset <- getOffset
      string "implicit"
      some spaceP
      t <- implicitTypeP
      some spaceP
      string "->"
      some spaceP
      retT <- typeP
      return $ U.BindImplicit offset t retT


emptyTupleP :: Parser U.Type
emptyTupleP = 
  label "empty tuple" $ do
    offset <- getOffset
    char '('
    many spaceP
    char ')'
    return $ U.TupleType offset []


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
typeNameP = 
  label "type name" $
    do
    offset <- getOffset
    name <- nameP
    return $ U.TypeName offset name

typeP :: Parser U.Type
typeP = 
  let 

    linearFunction = Expr.InfixR $ try $ label "-* function" $ do 
      offset <- getOffset
      many spaceP
      string "-*"
      many spaceP
      return $ U.LinearFunctionType offset

    pureFunction = Expr.InfixR $ try $ label "-> function" $ do 
      offset <- getOffset
      many spaceP
      string "->"
      many spaceP
      return $ U.FunctionType offset

    typeLambda = Expr.Prefix $ try $ label "type lambda" $ do
      offset <- getOffset
      name <- nameP
      many spaceP
      string "=>"
      many spaceP
      return $ U.TypeLambda offset name
  in 
    Expr.makeExprParser typeTermP [
      [Expr.InfixL $ try $ typeApplyP], 
      [pureFunction, linearFunction], 
      [typeLambda]
    ]


typeApplyP = 
  label "type application" $ 
    do
      offset <- getOffset
      some spaceP
      notFollowedBy $ choice 
        [
          fmap (const ()) $ oneOf $ ":",
          fmap (const ()) $ eol,
          fmap (const ()) $ choice [string "->", string "-*", string "=>"]
        ]
      return $ U.TypeApply offset

-- Matches a type term or type application. 
implicitTypeP =
  Expr.makeExprParser typeTermP [
      [Expr.InfixL $ try $ typeApplyP]
  ]
     
