module TypeCheck (typeCheck, TypeError (..)) where

import Control.Monad (sequence)
import qualified AST.Contextual as C
import qualified AST.Typed as T

data TypeError = WrongType Int T.Type T.Type 
  deriving (Eq, Show)

typeCheck :: C.Module -> Either TypeError T.Module
typeCheck m = do
  declarations <- sequence (fmap toTypedDeclaration (C.moduleDeclarations m))
  return $ T.Module (C.moduleName m) declarations

toTypedDeclaration :: C.Declaration -> Either TypeError T.Declaration
toTypedDeclaration (C.ValueDeclaration offset name declaredType expr) =
  let 
    inferredType = inferType expr
    typedDeclaredType = toTypedType declaredType
  in
    if inferredType == typedDeclaredType then 
      Right(T.ValueDeclaration name declaredType (toTypedExpression expr))
    else
      Left (WrongType (C.expressionOffset expr) typedDeclaredType inferredType)

toTypedExpression :: C.Expression -> T.Expression
toTypedExpression (C.IntLiteral _ value) = T.IntLiteral value
toTypedExpression (C.StringLiteral _ value) = T.StringLiteral value

inferType :: C.Expression -> T.Type
inferType (C.IntLiteral _ _) = T.BuiltInInt
inferType (C.StringLiteral _ _) = T.BuiltInString

toTypedType (C.Named _ "Int") = T.BuiltInInt
toTypedType (C.Named _ "String") = T.BuiltInString
