module MiniTriangle where

data Expr = LiteralInt Int | LiteralBool Bool | Var Identifier
          | BinOp BinaryOperator Expr Expr
          | UnOp UnaryOperator Expr
          | Conditional Expr Expr Expr
          | Apply Identifier [Expr]
  deriving (Show)

data BinaryOperator = Addition    | Subtraction      | Multiplication | Division
                    | LessThan    | LessThanEqual
                    | GreaterThan | GreaterThanEqual
                    | Equal       | NotEqual
                    | Conjunction | Disjunction
  deriving (Show)

data UnaryOperator = Negation | Not
  deriving (Show)

data Type = TInt | TBool
  deriving (Show, Eq)

data Command = Assign Identifier Expr
             | IfThenElse Expr Command Command
             | While Expr Command
             | GetInt Identifier
             | PrintInt Expr
             | BeginEnd [Command]
  deriving (Show)

data Declaration = VarDecl Identifier Type | VarInit Identifier Type Expr
                 | FunDecl Identifier [TypeContext] Type Expr
  deriving (Show)

data Program = LetIn [Declaration] Command
  deriving (Show)

type Identifier = String
type TypeContext = (Identifier, Type)
