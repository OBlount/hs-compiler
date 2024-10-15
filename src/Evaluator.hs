module Evaluator where

import Parser (AST(..), BinaryOperator(..))

eval :: AST -> Integer
eval = evaluate

evaluate :: AST -> Integer
evaluate (LitInteger x) = x
evaluate (BinOp op left right) =
  let leftValue  = evaluate left
      rightValue = evaluate right
  in case op of
    Addition       -> leftValue + rightValue
    Subtraction    -> leftValue - rightValue
    Multiplication -> leftValue * rightValue
    Division       -> leftValue `div` rightValue
