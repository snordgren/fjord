module Parsing.Basics where

import Text.Megaparsec
import Text.Megaparsec.Char


type Parser = Parsec String String


nameP :: Parser String
nameP = some letterChar


qualifiedNameP :: Parser String
qualifiedNameP = do
  head <- some letterChar
  tail <- many (letterChar <|> (char '.'))
  return (head ++ tail)


spaceP :: Parser Char
spaceP = oneOf " \t\r"


spaceInExpressionP :: Parser ()
spaceInExpressionP = do
  many spaceP
  option () $ do
    eol
    many spaceP
    return ()
  return ()

