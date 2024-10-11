module Evaluator where

import Data.Maybe (fromMaybe)

import Scanner (scan)
import Parser (parse, AST(..), BinaryOperator(..))

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

main :: IO ()
main = do
  print $ fromIntegral $ eval $ fst $ fromMaybe (error "Parsing failed") (parse $ scan "7 + (10/3) * 2")
