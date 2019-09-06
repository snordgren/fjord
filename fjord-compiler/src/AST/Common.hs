module AST.Common where


data Origin
  = SameModule
  | OtherModule String
  | InFunction
  deriving (Eq, Show)


data Uniqueness 
  = Unique
  | NonUnique
  deriving (Eq, Show)


data NameType
  = TypeVar
  | TypeRef
  deriving (Eq, Show)


uniqPrefix :: Uniqueness -> String
uniqPrefix a =
  case a of 
    Unique -> "1:"
    NonUnique -> "?:"
