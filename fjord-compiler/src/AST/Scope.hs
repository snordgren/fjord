module AST.Scope where

import qualified Data.List as List

import AST.Common (Type (..))
import qualified AST.Common as Common

type ScopeValue = (String, Type, Common.Origin, [Type])

data Scope
  = Scope { 
    scopeValues :: [ScopeValue],
    scopeTypes :: [(String, Common.Origin, Common.NameType)],
    scopeImplicits :: [(String, Type, Common.Origin)],
    -- | A list of fields with values name, target record type, field type, and origin.
    scopeFields :: [(String, Type, Type, Common.Origin)]
  }
  deriving (Show)


{-|
Merge two scopes, the tightest bound (innermost) scope should come first. 
-}
mergeScope :: Scope -> Scope -> Scope
mergeScope a b = 
  let 
    mergedValues = scopeValues a ++ scopeValues b
    mergedTypes = scopeTypes a ++ scopeTypes b
    mergedImplicits = scopeImplicits a ++ scopeImplicits b
    mergedFields = scopeFields a ++ scopeFields b
  in
    Scope mergedValues mergedTypes mergedImplicits mergedFields
