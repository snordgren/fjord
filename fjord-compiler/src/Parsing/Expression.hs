module Parsing.Expression (expressionP) where
  
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char

import Parsing.Basics
import qualified AST.Contextual as C

expressionP = 
  label "expression" $ makeExprParser termP 
    [
      [InfixL (try applyP)], 
      [InfixL (try additionP)]
    ]

termP :: Parser C.Expression
termP = 
  (try lambdaP) <|> recordUpdateP <|> intLiteralP <|> stringLiteralP <|> nameExpressionP


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
recordUpdateP = do
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
intLiteralP = do
  offset <- getOffset
  num <- some (oneOf "1234567890")
  return $ C.IntLiteral offset (read num :: Integer)


stringLiteralP :: Parser C.Expression
stringLiteralP = do
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


applyP = do
  offset <- getOffset
  some spaceP
  notFollowedBy $ choice 
    [
      fmap (\_ -> ()) additionP, 
      fmap (\_ -> ()) $ oneOf "}|"
    ]
  return $ C.Apply offset


additionP = do
  offset <- getOffset
  many spaceP
  char '+'
  many spaceP
  return $ C.Addition offset
