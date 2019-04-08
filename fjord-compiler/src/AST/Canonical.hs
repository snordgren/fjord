module AST.Canonical where

data Module = Module { moduleName :: String, moduleDeclarations :: [Declaration] }
data Expression = IntLiteral Int Integer | StringLiteral Int String
data Declaration = ValueDeclaration Int String Type Expression

data Type = Canonical String String |
  BuiltInInt |
  BuiltInString
  deriving (Eq, Show)

expressionOffset :: Expression -> Int
expressionOffset (IntLiteral offset _) = offset
expressionOffset (StringLiteral offset _) = offset
