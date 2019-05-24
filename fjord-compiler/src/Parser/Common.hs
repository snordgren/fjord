{-# LANGUAGE FlexibleInstances #-}
module Parser.Common (
  keySym,
  keyword,
  keywords,
  nameP,
  operatorNameP,
  opSym,
  qualifiedNameP,
  reservedSym,
  spaceP,
  spaceInExpressionP,
  Parser
) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.List as List
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec String String


keywords = 
  ["as", "case", "do", "enum", "forall", "implicit", "in", "let", "module", "of", "record", "use"]


opSym = 
  "+-/*^:<>&|?@~=%!"


reservedSym = 
  ["->", "<-", ":", "=", "=>", "|", "&", "-*", "*-"]

keySym = 
  "})"


keyword :: String -> Parser String
keyword s = 
  try $ do
    string s
    notFollowedBy alphaNumChar
    return s


nameP :: Parser String
nameP = label "name" $ do
  s <- some letterChar
  t <- many alphaNumChar
  let n = s ++ t
  if elem n keywords then
    fail (n ++ " is a keyword")
  else
    return n


operatorNameP :: Parser String
operatorNameP = label "operator name" $ do
  char '('
  s <- some $ oneOf opSym
  char ')'
  if elem s reservedSym then
    fail $ s ++ " is a reserved symbol"
  else
    return $ "(" ++ s ++ ")"

qualifiedNameP :: Parser String
qualifiedNameP = label "qualified name" $ do
  head <- nameP
  tail <- try $ many (char '.' >> nameP)
  return $ List.intercalate "." (head : (if length tail > 0 then [List.intercalate "." tail] else []))

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


instance ShowErrorComponent String where
  showErrorComponent = id
  errorComponentLen = length

