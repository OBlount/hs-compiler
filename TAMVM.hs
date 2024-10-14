module TAMVM where

type Stack = [Int]

data TAMInst
  = LOADL Int
  | ADD
  | SUB
  | MUL
  | DIV
  | NEG
  deriving (Show)

execute :: Stack -> TAMInst -> Maybe Stack
execute stack (LOADL x) = undefined

main :: IO ()
main = do
  let stack = execute [] (LOADL 123)
  case stack of
    Just stack -> print stack
    Nothing    -> putStrLn "error"
