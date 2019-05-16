module Check.Types.InferUnique where

import qualified Data.List as List


import Check.Scope
import Check.Types.Types
import qualified AST.Common as Common
import qualified AST.Untyped as U


inferExprUniq :: U.Scope -> Common.Uniqueness -> U.Expression -> Either TypeError Common.Uniqueness
inferExprUniq scope expectUniq expr = 
  case expr of 

    U.IntLiteral _ _ ->
      return expectUniq

    U.Name offset name -> 
      do
        (typ, uniq, orig) <- scopeVariableType scope offset name
        return uniq

    _ -> 
      error ("not yet implemented for " ++ (show expr))


resolveTupleUniq :: Int -> [Common.Uniqueness] -> Either TypeError Common.Uniqueness
resolveTupleUniq offset uniqValues =
  if List.length uniqValues == 0 then
    return Common.Unique
  else if List.length (List.nub uniqValues) == 1 then
    return $ List.head uniqValues
  else 
    Left $ MixedUniquenessInTuple offset
