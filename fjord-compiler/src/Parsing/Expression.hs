module Parsing.Expression (
  caseP, 
  expressionP, 
  patternP
) where
  
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Control.Monad as Monad

import Parsing.Basics
import qualified AST.Untyped as U


expressionP :: Parser U.Expression
expressionP = 
  let 
    mkOps xs = fmap (\a -> InfixL $ try $ operatorP a) xs
  in 
    label "expression" $ makeExprParser termP 
      [
        [InfixL $ try applyP], 
        mkOps ['*', '/', '%'],
        mkOps ['+', '-'],
        mkOps [':'],
        mkOps ['<', '>'],
        mkOps ['=', '!'],
        mkOps ['&'],
        mkOps ['^'],
        mkOps ['|']
      ]

termP :: Parser U.Expression
termP = 
  choice [
    (try caseP), (try lambdaP), (try tupleExprP), recordUpdateP, intLiteralP, stringLiteralP, 
    nameExpressionP, parenExprP
  ]


lambdaP :: Parser U.Expression
lambdaP = label "lambda" $ do
  offset <- getOffset
  name <- nameP
  many spaceP
  string "->"
  many spaceP
  ret <- expressionP
  return $ U.Lambda offset name ret


recordUpdateP :: Parser U.Expression
recordUpdateP = label "record update" $ do
  offset <- getOffset
  char '{'
  many spaceP
  target <- expressionP
  many spaceP
  char '|'
  many spaceP
  updateHead <- fieldUpdateP
  updateTail <- many ((char ',') >> (many spaceP) >> fieldUpdateP)
  many spaceP
  char '}'
  let updates = updateHead : updateTail
  return $ U.RecordUpdate offset target updates


fieldUpdateP :: Parser U.FieldUpdate
fieldUpdateP = do
  offset <- getOffset
  fieldName <- nameP
  many spaceP
  char '='
  many spaceP
  value <- expressionP
  return $ U.FieldUpdate offset fieldName value


intLiteralP :: Parser U.Expression
intLiteralP = label "integer" $ do
  offset <- getOffset
  num <- some (oneOf "1234567890")
  return $ U.IntLiteral offset (read num :: Integer)


stringLiteralP :: Parser U.Expression
stringLiteralP = label "string" $ do
  offset <- getOffset
  char '"'
  strings <- many stringPartP 
  char '"'
  return $ U.StringLiteral offset (concat strings)


nonEscape :: Parser String
nonEscape = do
  d <- char '\\'
  c <- oneOf "\\\"0nrvtbf"
  return [d, c]


escape :: Parser String
escape = some $ noneOf "\\\"\0\n\r\v\t\b\f"


stringPartP :: Parser String
stringPartP = nonEscape <|> escape


nameExpressionP :: Parser U.Expression
nameExpressionP = do
  offset <- getOffset
  name <- nameP
  return $ U.Name offset name


tupleExprP :: Parser U.Expression
tupleExprP = 
  let 
    rhsP = do
      many spaceP
      char ',' 
      many spaceP
      expr <- expressionP
      return $ expr
  in do
    offset <- getOffset
    char '('
    many spaceP
    head <- expressionP
    tail <- some $ try rhsP
    char ')'
    return $ U.Tuple offset $ head : tail


parenExprP :: Parser U.Expression
parenExprP = do
  char '('
  spaceInExpressionP
  innerExpression <- expressionP
  spaceInExpressionP
  char ')'
  return innerExpression


applyP = do
  offset <- getOffset
  some spaceP
  notFollowedBy $ choice 
    [
      fmap (const ()) $ oneOf $ "})," ++ opSym,
      fmap (const ()) $ string "of"
    ]
  return $ U.Apply offset


{-|
Matches operators starting with character c.
-}
operatorP :: Char -> Parser (U.Expression -> U.Expression -> U.Expression)
operatorP c = do
  offset <- getOffset
  many spaceP
  head <- char c
  tail <- many $ oneOf opSym
  many spaceP
  let name = head : tail
  if elem name reservedSym then
    fail $ name ++ " is a reserved symbol"
  else 
    return $ U.Operator offset name


caseP = label "case expression" $ do
  offset <- getOffset
  keyword "case"
  spaceInExpressionP
  expr <- expressionP
  spaceInExpressionP
  keyword "of"
  many spaceP
  eol
  patterns <- some (try patternP)
  return $ U.Case offset expr patterns


patternP = label "pattern" $ do
  spaceInExpressionP
  offset <- getOffset
  constructor <- qualifiedNameP
  variables <- many (try ((some spaceP) >> nameP))
  many spaceP
  string "->"
  many spaceP
  expr <- expressionP
  eol
  return $ U.Pattern offset constructor variables expr
