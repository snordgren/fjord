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
  | TypeName Int String
  deriving (Eq)

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

      TypeName pos s -> 
        s
