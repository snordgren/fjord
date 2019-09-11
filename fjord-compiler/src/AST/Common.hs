module AST.Common where


data Origin
  = SameModule
  | OtherModule String
  | InFunction
  deriving (Eq, Show)


data NameType
  = TypeVar
  | TypeRef
  deriving (Eq, Show)
