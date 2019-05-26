module Transform.ToHybrid.Type (
  transformType,
) where

import qualified AST.Hybrid as H
import qualified AST.Typed as T

transformType :: T.Type -> H.Type
transformType t =
  case t of 
    T.TypeName _ name _ ->
      H.TypeName name
  
    _ ->
      error $ "missing pattern for " ++ show t
      