module Transform.ToHybrid.Type (
  transformType,
) where

import qualified AST.Hybrid as H
import qualified AST.Typed as T

transformType :: Type -> H.Type
transformType t =
  case t of 
    TypeName name nameType ->
      H.TypeName name
  
    _ ->
      error $ "missing pattern for " ++ show t
      