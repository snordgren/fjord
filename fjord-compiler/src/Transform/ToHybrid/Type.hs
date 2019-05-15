module Transform.ToHybrid.Type (
  transformType,
) where

import qualified AST.Hybrid as H
import qualified AST.Typed as T

transformType :: T.Type -> H.Type
transformType (T.BuiltInInt _) = H.BuiltInInt
