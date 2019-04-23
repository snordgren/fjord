module CodeGen.TypeDef (genDefStr) where

import qualified Data.List as List

import qualified AST.Typed as T

{-|
Generates the text content of the .d.fj file. 
-}
genDefStr :: T.Module -> String
genDefStr m =
  "module " ++ (T.moduleName m) ++ "\n\n" ++ 
    (List.intercalate "\n\n" $ fmap genDeclDefStr $ T.moduleDeclarations m) ++ 
    "\n"


genDeclDefStr :: T.Declaration -> String
genDeclDefStr decl = 
  case decl of 

    T.EnumDeclaration name ctors ->
      let
        ctorStr = List.intercalate "\n" $ fmap genCtorStr ctors 
      in
        "enum " ++ name ++ "\n" ++ ctorStr

    T.RecordDeclaration name fields -> 
      let 
        fieldStrList :: [String]
        fieldStrList = 
           fmap genFieldStr fields
        
        fieldStr :: String
        fieldStr = 
          List.intercalate "\n" fieldStrList

      in
        "record " ++ name ++ "\n" ++ fieldStr

    T.ValueDeclaration name parameters typ expr ->
      name ++ " : " ++ (genTypeDefStr typ)


genCtorStr :: T.EnumConstructor -> String
genCtorStr (T.EnumConstructor name t) =
  "  " ++ name ++ " : " ++ genTypeDefStr t


genFieldStr :: T.RecordField -> String
genFieldStr (T.RecordField recordFieldName recordFieldType) = 
  "  " ++ recordFieldName ++ " : " ++ genTypeDefStr recordFieldType


genTypeDefStr :: T.Type -> String
genTypeDefStr t = 
  case t of
    T.BuiltInInt -> 
      "Int"

    T.BuiltInString -> 
      "String"

    T.FunctionType par ret -> 
      let 
        parStr = 
          case par of 
            T.FunctionType a b -> 
              "(" ++ genTypeDefStr par ++ ")"

            _ -> 
              genTypeDefStr par
      in
        parStr ++ " -> " ++ genTypeDefStr ret

    T.TupleType types -> 
      "(" ++ (List.intercalate ", " $ fmap genTypeDefStr types) ++ ")"

    T.TypeName a ->
      a
        