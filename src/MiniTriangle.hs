module MiniTriangle where

data Expr = LiteralInt Int | Var Identifier
          | BinOp BinaryOperator Expr Expr
          | UnOp UnaryOperator Expr
          | Conditional Expr Expr Expr
  deriving (Show)

data BinaryOperator = Addition    | Subtraction      | Multiplication | Division
                    | LessThan    | LessThanEqual
                    | GreaterThan | GreaterThanEqual
                    | Equal       | NotEqual
                    | Conjunction | Disjunction
  deriving (Show)

data UnaryOperator = Negation | Not
  deriving (Show)

data Command = IfThenElse Expr Command Command
             | While Expr Command
             | GetInt Identifier
             | PrintInt Expr
             | BeginEnd [Command]
  deriving (Show)

data Declaration = VarDecl Identifier | VarInit Identifier Expr
  deriving (Show)

data Program = LetIn [Declaration] Command
  deriving (Show)

type Identifier = String
