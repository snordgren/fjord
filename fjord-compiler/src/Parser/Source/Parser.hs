module Parser.Source.Parser (
  runModuleParser
) where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import qualified Data.List as List

import Parser.Common
import Parser.Source.Definition (defP)
import Parser.Source.Expression
import qualified AST.Common as Common
import qualified AST.Untyped as U


runModuleParser :: String -> String -> Either (ParseErrorBundle String String) U.Module
runModuleParser = 
  runParser moduleP

  
moduleP :: Parser U.Module
moduleP = label "module" $ do
  string "module"
  some spaceP
  moduleName <- qualifiedNameP
  many spaceP
  some eol
  imports <- many importP
  defs <- some defP
  return $ U.Module moduleName imports defs


importP :: Parser U.Import
importP = label "import" $ do
  string "import"
  some spaceP
  offset <- getOffset
  moduleName <- qualifiedNameP
  many spaceP
  source <- option Nothing $ do
    string "from"
    s <- stringP
    return $ Just s

  some eol
  return $ U.Import offset moduleName source
