module TAMVM where

import Parser (AST(..), BinaryOperator(..), UnaryOperator(..))

type Stack = [Integer]

data TAMInst
  = LOADL Integer
  | ADD
  | SUB
  | MUL
  | DIV
  | NEG
  deriving (Show)

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

execute :: Stack -> TAMInst -> Maybe Stack
execute stack (LOADL x) = Just (x:stack)
execute (x:y:stack) ADD = Just ((x+y)    :stack)
execute (x:y:stack) SUB = Just ((x-y)    :stack)
execute (x:y:stack) MUL = Just ((x*y)    :stack)
execute (x:y:stack) DIV = Just ((x`div`y):stack)
execute (x:stack)   NEG = Just ((-x)     :stack)
execute _ _             = Nothing

executeTAM :: Stack -> [TAMInst] -> Maybe Stack
executeTAM stack []     = Just stack
executeTAM stack (i:is) = case execute stack i of
                            Just stack' -> executeTAM stack' is
                            Nothing     -> Nothing

main :: IO ()
main = do
  -- Test instruction set
  putStrLn "test 1: test execution of instruction set"
  let instructions = [LOADL 3, LOADL 5, MUL]
  case executeTAM [] instructions of
    Just res   -> print res
    Nothing    -> putStrLn "error - failed to execute TAM"
  -- Test code generation
  putStrLn "test 2: test code generation"
  let tree = BinOp Multiplication (LitInteger 3) (BinOp Addition (LitInteger 3) (LitInteger 2))
  let generatedCode = expCode tree
  print generatedCode
  -- Test code generation evaluation
  putStrLn "test 3: test code generation evaluation"
  case executeTAM [] generatedCode of
    Just res   -> print res
    Nothing    -> putStrLn "error - failed to execute generated TAM"
