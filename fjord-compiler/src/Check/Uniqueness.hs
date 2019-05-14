module Check.Uniqueness (check) where

import qualified AST.Untyped as U

data UniquenessError 
  = TooManyUsages Int String
  | TooFewUsages Int String


check :: [U.TypeDef] -> U.Module -> Maybe UniquenessError 
check typeDefs mod =
  Nothing
