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
import qualified AST.Contextual as C


expressionP :: Parser C.Expression
expressionP = 
  label "expression" $ makeExprParser termP 
    [
      [InfixL (try applyP)], 
      [InfixL (try additionP)]
    ]

termP :: Parser C.Expression
termP = 
  choice [
    (try caseP), (try lambdaP), recordUpdateP, intLiteralP, stringLiteralP, nameExpressionP,
    parenthesizedExpressionP
  ]


lambdaP :: Parser C.Expression
lambdaP = do
  offset <- getOffset
  name <- nameP
  many spaceP
  string "->"
  many spaceP
  ret <- expressionP
  return $ C.Lambda offset name ret


recordUpdateP :: Parser C.Expression
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
  return $ C.RecordUpdate offset target updates


fieldUpdateP :: Parser C.FieldUpdate
fieldUpdateP = do
  offset <- getOffset
  fieldName <- nameP
  many spaceP
  char '='
  many spaceP
  value <- expressionP
  return $ C.FieldUpdate offset fieldName value


intLiteralP :: Parser C.Expression
intLiteralP = label "integer" $ do
  offset <- getOffset
  num <- some (oneOf "1234567890")
  return $ C.IntLiteral offset (read num :: Integer)


stringLiteralP :: Parser C.Expression
stringLiteralP = label "string" $ do
  offset <- getOffset
  char '"'
  strings <- many stringPartP 
  char '"'
  return $ C.StringLiteral offset (concat strings)


nonEscape :: Parser String
nonEscape = do
  d <- char '\\'
  c <- oneOf "\\\"0nrvtbf"
  return [d, c]


escape :: Parser String
escape = some $ noneOf "\\\"\0\n\r\v\t\b\f"


stringPartP :: Parser String
stringPartP = nonEscape <|> escape


nameExpressionP :: Parser C.Expression
nameExpressionP = do
  offset <- getOffset
  name <- nameP
  return $ C.Name offset name


parenthesizedExpressionP :: Parser C.Expression
parenthesizedExpressionP = do
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
      fmap (const ()) additionP, 
      fmap (const ()) $ oneOf "}|",
      fmap (const ()) $ string "of"
    ]
  return $ C.Apply offset


additionP = do
  offset <- getOffset
  many spaceP
  char '+'
  many spaceP
  return $ C.Addition offset


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
  return $ C.Case offset expr patterns


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
  return $ C.Pattern offset constructor variables expr
