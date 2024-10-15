module CodeGenerator where

import Parser (AST(..), BinaryOperator(..), UnaryOperator(..))
import TAMVM (Stack(..), TAMInst(..))

expCode :: AST -> [TAMInst]
expCode (LitInteger x)        = [LOADL x]
expCode (BinOp op left right) =
  let left'  = expCode left
      right' = expCode right
  in case op of
    Addition       -> left' ++ right' ++ [ADD]
    Subtraction    -> left' ++ right' ++ [SUB]
    Multiplication -> left' ++ right' ++ [MUL]
    Division       -> left' ++ right' ++ [DIV]

main :: IO ()
main = do
  let tree = BinOp Multiplication (LitInteger 3) (BinOp Addition (LitInteger 3) (LitInteger 2))
  let generatedCode = expCode tree
  print generatedCode
