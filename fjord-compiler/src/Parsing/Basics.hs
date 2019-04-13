module Parsing.Basics (
  keyword,
  nameP,
  qualifiedNameP,
  spaceP,
  spaceInExpressionP,
  Parser
) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer


type Parser = Parsec String String


keywords = ["as", "case", "do", "enum", "let", "module", "of", "record", "use"]


keyword :: String -> Parser String
keyword s = 
  try $ do
    string s
    notFollowedBy alphaNumChar
    return s


nameP :: Parser String
nameP = do
  s <- some letterChar
  if elem s keywords then
    fail (s ++ " is a keyword")
  else
    return s


qualifiedNameP :: Parser String
qualifiedNameP = do
  head <- some letterChar
  tail <- many (letterChar <|> (char '.'))
  return (head ++ tail)


spaceP :: Parser ()
spaceP = do 
  oneOf " \r\t"
  return ()


spaceInExpressionP :: Parser ()
spaceInExpressionP = do
  many spaceP
  option () $ do
    eol
    many spaceP
    return ()
  return ()

