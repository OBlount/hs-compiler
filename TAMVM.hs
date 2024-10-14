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
  let instructions = [LOADL 3, LOADL 5, MUL]
  case executeTAM [] instructions of
    Just res   -> print res
    Nothing    -> putStrLn "error - failed to execute TAM"
