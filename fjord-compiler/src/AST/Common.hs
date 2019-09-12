module AST.Common where

import qualified Data.List as List


data Origin
  = SameModule
  | OtherModule String
  | InFunction
  deriving (Eq, Show)


data NameType
  = TypeVar
  | TypeRef
  deriving (Eq, Show)

data Type 
  = FunctionType Int Type Type 
  | TupleType Int [Type]
  | TypeApply Int Type Type
  | TypeLambda Int String Type
  | TypeName Int String NameType

instance Show Type where
  show a =
    case a of 
      FunctionType pos p r ->
        "(" ++ show p ++ " -> " ++ show r ++ ")"

      TupleType pos values -> 
        "(" ++ List.intercalate ", " (fmap show values) ++ ")"

      TypeApply pos f par ->
        "(" ++ show f ++ " " ++ show par ++ ")"

      TypeLambda pos arg ret -> 
        arg ++ ". " ++ show ret

      TypeName pos s nameType -> 
        s


compareTypEq :: Type -> Type -> Bool
compareTypEq a b = 
  case a of 
    FunctionType _ c d -> 
      case b of 
        FunctionType _ e f -> 
          (compareTypEq c e) && (compareTypEq d f)

        _ -> 
          False

    TypeApply _ af ap -> 
      case b of 
        TypeApply _ bf bp ->
          compareTypEq af bf && compareTypEq ap bp
        _ -> False

    TypeName _ c cNameType ->
      case b of 
        TypeName _ d dNameType-> 
          c == d && cNameType == dNameType

        _ ->
          False

    TupleType _ c ->
      case b of 
        TupleType _ d -> 
          if null c && null d then True else 
            (List.length c == List.length d) &&
              (List.elem True $ zipWith (\x y -> compareTypEq x y) c d)

        _ ->
          False


    _ -> 
      error $ "Missing pattern for " ++ (show a)
