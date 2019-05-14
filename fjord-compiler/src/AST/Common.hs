module AST.Common where


data Origin
  = SameModule
  | OtherModule String
  deriving (Eq, Show)


data Uniqueness 
  = Unique
  | NonUnique
  deriving (Eq, Show)
