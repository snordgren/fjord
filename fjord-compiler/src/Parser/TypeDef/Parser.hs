module Parser.TypeDef.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char

import Parser.Common
import Parser.Declaration (enumDeclP, recDeclP, valDeclP)
import qualified AST.Untyped as U

typeDefP :: Parser U.TypeDef
typeDefP = 
  do
    string "module"
    some spaceP
    name <- qualifiedNameP
    many spaceP
    some eol
    decls <- some declP
    return $ U.TypeDef name decls


declP :: Parser U.Declaration
declP = 
  choice 
    [
      fmap U.DeclEnumDecl enumDeclP,
      fmap U.DeclRecDecl recDeclP,
      fmap U.DeclValDecl valDeclP
    ]

