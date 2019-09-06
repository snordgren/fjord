module AST.Scope where

import qualified AST.Common as Common

type ScopeValue a = (String, a, Common.Origin, [a])

data Scope a
  = Scope { 
    scopeValues :: [ScopeValue a],
    scopeTypes :: [(String, Common.Origin, Common.NameType)],
    scopeImplicits :: [(String, a, Common.Origin)],
    -- | A list of fields with values name, target record type, field type, and origin.
    scopeFields :: [(String, a, a, Common.Origin)]
  }
  deriving (Eq, Show)


{-|
Merge two scopes, the tightest bound (innermost) scope should come first. 
-}
mergeScope :: Scope a -> Scope a -> Scope a
mergeScope a b = 
  let 
    mergedValues = scopeValues a ++ scopeValues b
    mergedTypes = scopeTypes a ++ scopeTypes b
    mergedImplicits = scopeImplicits a ++ scopeImplicits b
    mergedFields = scopeFields a ++ scopeFields b
  in
    Scope mergedValues mergedTypes mergedImplicits mergedFields
