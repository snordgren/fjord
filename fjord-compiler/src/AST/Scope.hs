module AST.Scope where

import qualified AST.Common as Common

data Scope a
  = Scope { 
    scopeValues :: [(String, a, Common.Uniqueness, Common.Origin)],
    scopeTypes :: [(String, Common.Origin, Common.NameType)],
    scopeImplicits :: [(String, a, Common.Origin)],
    -- | A list of fields with values name, target record type, field type, and origin.
    scopeFields :: [(String, a, a, Common.Origin)]
  }
  deriving (Eq, Show)
