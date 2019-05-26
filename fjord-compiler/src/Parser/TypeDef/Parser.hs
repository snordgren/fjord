module Parser.TypeDef.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char

import Parser.Common
import Parser.Declaration (enumDeclP, implicitDeclP, recDeclP, valDeclP)
import qualified AST.Untyped as U

typeDefP :: String -> Parser U.TypeDef
typeDefP fileName = 
  do
    string "module"
    some spaceP
    name <- qualifiedNameP
    many spaceP
    some eol
    decls <- some declP
    return $ U.TypeDef fileName name decls


declP :: Parser U.Declaration
declP = 
  choice 
    [
      fmap U.DeclEnumDecl enumDeclP,
      fmap U.DeclImplicitDecl implicitDeclP,
      fmap U.DeclRecDecl recDeclP,
      fmap U.DeclValDecl valDeclP
    ]

