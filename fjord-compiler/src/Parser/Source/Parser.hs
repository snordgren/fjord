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
import qualified AST.Untyped as U


runModuleParser :: String -> String -> Either (ParseErrorBundle String String) U.Module
runModuleParser = 
  runParser moduleP

  
moduleP :: Parser U.Module
moduleP = do
  string "module"
  some spaceP
  moduleName <- qualifiedNameP
  many spaceP
  some eol
  defs <- some defP
  return $ U.Module moduleName defs
