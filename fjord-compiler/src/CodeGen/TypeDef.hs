module CodeGen.TypeDef (genDefStr) where

import qualified Data.List as List

import AST.Common (Type (..))
import qualified AST.Typed as T

{-|
Generates the text content of the .d.fj file. 
-}
genDefStr :: T.Module -> String
genDefStr m =
  "module " ++ (T.moduleName m) ++ "\n\n" ++ 
    (List.intercalate "\n\n" $ fmap defToTypeDefStr $ T.moduleDefs m) ++ 
    "\n"


defToTypeDefStr :: T.Definition -> String
defToTypeDefStr decl = 
  case decl of 

    T.EnumDef name ctors ->
      let
        ctorStr = List.intercalate "\n" $ fmap genCtorStr ctors 
      in
        "enum " ++ name ++ "\n" ++ ctorStr

    T.ImplicitDef name typ expr ->
      "implicit\n" ++ name ++ " : " ++ genTypeDefStr typ

    T.RecDef name fields -> 
      let 
        fieldStrList :: [String]
        fieldStrList = 
           fmap genFieldStr fields
        
        fieldStr :: String
        fieldStr = 
          List.intercalate "\n" fieldStrList

      in
        "record " ++ name ++ "\n" ++ fieldStr

    T.ValDef name params typ expr ->
      name ++ " : " ++ (genTypeDefStr typ)


genCtorStr :: T.EnumConstructor -> String
genCtorStr (T.EnumConstructor name parTypes retType) =
  let
    parS = 
      if List.length parTypes == 0 then
        ""
      else
        (List.intercalate " " $ fmap genTypeDefStr parTypes) ++ " "

    retS =
      genTypeDefStr retType
  in
    "  " ++ name ++ " " ++ parS ++ ": " ++ retS


genFieldStr :: T.RecField -> String
genFieldStr (T.RecField recFieldName recFieldType) = 
  "  " ++ recFieldName ++ " : " ++ genTypeDefStr recFieldType


genTypeDefStr :: Type -> String
genTypeDefStr t = 
  case t of
    FunctionType _ par ret -> 
      let 
        parStr = 
          case par of 
            FunctionType _ a b -> 
              "(" ++ genTypeDefStr par ++ ")"

            _ -> 
              genTypeDefStr par
      in
        parStr ++ " -> " ++ genTypeDefStr ret

    TupleType _ types -> 
      "(" ++ (List.intercalate ", " $ fmap genTypeDefStr types) ++ ")"

    TypeApply _ f par ->
      genTypeDefStr f ++ " " ++ genTypeDefStr par

    TypeLambda _ var ret ->
      var ++ ". " ++ genTypeDefStr ret

    TypeName pos a nameType ->
      a
