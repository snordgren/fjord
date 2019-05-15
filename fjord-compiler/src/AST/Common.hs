module AST.Common where


data Origin
  = SameModule
  | OtherModule String
  deriving (Eq, Show)


data Uniqueness 
  = Unique
  | UniqueTopLevel -- unique, but does not need to be used in an expression
  | NonUnique
  deriving (Eq, Show)
