{-|
An AST intended to be an intermediate format for a language with some support 
for functional programming, such as JavaScript. This AST can be used to either 
generate JS directly or it can be lowered further to a more imperative form.

No correctness checking should be necessary in this format. 
-}
module AST.Hybrid where

data Source = 
  Source {
    sourceName :: String,
    sourceDefinitions :: [Definition]
  } 
  deriving (Eq, Show)

data Definition 
  = FunctionDefinition String [(String, Type)] Type FunctionBody
  | ValueDefinition String Type Expression
  deriving (Eq, Show)

data FunctionBody
  = BlockFunctionBody Block
  | SimpleFunctionBody Expression
  deriving (Eq, Show)

data Type 
  = BuiltInInt
  | BuiltInString
  | FunctionType Type Type
  | TypeName String
  deriving (Eq, Show)

data Expression
  -- | Primitive addition between two expressions of a specific type. 
  = Addition Type Expression Expression
  | Allocate Type
  | Array [Expression]
  {-
    An array access. The first expression returns the array and the second
    expression returns the index of the element to access. 
  -}
  | ArrayAccess Expression Expression
  {-|
    An immediately invoked function expression, used when we need to use 
    features only statements can have, but need to return an expression, 
    such as in mutating a record. 
  -}
  | Equals Expression Expression
  | IIFE Block
  {-|
    Prevent the result of this expression from being mutated. 
  -}
  | Immutable Expression
  | IntLiteral Integer
  -- | Invoke a function with a set of expressions. This must be a 
  -- | complete invocation of the function. 
  | Invoke Expression [Expression]
  | Lambda [(String, Type)] Expression
  | Read Type String 
  | StringLiteral String
  deriving (Eq, Show)

data Block 
  = Block {
    blockDeclarations :: [(String, Type)],
    blockStatements :: [Statement]
  }
  deriving (Eq, Show)

data Statement 
  = Assign String Expression
  | If [(Expression, Block)] (Maybe Block)
  | Mutate Expression String Expression
  | Return Expression
  deriving (Eq, Show)
