module TAMVM where

type Stack = [Integer]

data TAMInst
  = LOADL Integer
  | ADD
  | SUB
  | MUL
  | DIV
  | NEG
  deriving (Show)

executeTAM :: Stack -> [TAMInst] -> Stack
executeTAM stack []     = stack
executeTAM stack (i:is) = case execute stack i of
                            Just stack' -> executeTAM stack' is
                            Nothing     -> error $ "TAMVM: execution failed on instruction: " ++ show i

execute :: Stack -> TAMInst -> Maybe Stack
execute stack (LOADL x) = Just (x:stack)
execute (x:y:stack) ADD = Just ((x+y)    :stack)
execute (x:y:stack) SUB = Just ((x-y)    :stack)
execute (x:y:stack) MUL = Just ((x*y)    :stack)
execute (x:y:stack) DIV = Just ((x`div`y):stack)
execute (x:stack)   NEG = Just ((-x)     :stack)
execute _ _             = Nothing

main :: IO ()
main = do
  let instructions = [LOADL 3, LOADL 5, MUL]
  print $ executeTAM [] instructions
