module TAMVM where

import TAMState (TAMState(..), Stack, PC)
import TAMState ((!|!), continue, stackPush, stackPop, stackUpdate, findLabel, updateCounter, stackNextInstruction)
import STIO (StateIO(..), stState, stUpdate, lift)
import TAMCode (Instruction(..))

run :: StateIO TAMState a -> TAMState -> IO a
run (StT st) initialState = do
  (ret, _) <- st initialState
  return ret

execute :: [Instruction] -> StateIO TAMState ()
execute [] = return ()
execute (i:is) = do
  executeInstruction i
  execute is

executeInstruction :: Instruction -> StateIO TAMState ()
executeInstruction (LOAD a)      = do
  state <- stState
  let x = (tsStack state) !|! a
  stackPush x
  continue
executeInstruction (STORE addr)  = do
  x     <- stackPop
  state <- stState
  let updatedStack = stackUpdate (tsStack state) addr x
  stUpdate state { tsStack = updatedStack }
  continue
executeInstruction (LOADL x)    = do
  stackPush x
  continue
executeInstruction (GETINT)     = do
  lift (putStrLn "Enter a number: ")
  n <- lift getLine
  stackPush (read n)
  continue
executeInstruction (PUTINT)     = do
  n <- stackPop
  lift (putStrLn ("Output> " ++ (show n)))
  continue
executeInstruction (JUMP id)    = do
  state              <- stState
  instructionAddress <- findLabel id
  updateCounter instructionAddress
executeInstruction (JUMPIFZ id) = do
  state              <- stState
  instructionAddress <- findLabel id
  p                  <- stackPop
  if p == 0 then updateCounter instructionAddress
            else continue
executeInstruction (Label id)   = return ()
executeInstruction (HALT)       = return ()
executeInstruction (ADD)        = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush (x+y)
  continue
executeInstruction (SUB)        = do
  state <- stState
  y     <- stackPop
  x     <- stackPop
  stackPush (x-y)
  continue
executeInstruction (MUL)        = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush (x*y)
  continue
executeInstruction (DIV)        = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush (x `div` y)
  continue
executeInstruction (LSS)        = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush ((if (x < y) then 1 else 0))
  continue
executeInstruction (GRT)        = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush ((if (x > y) then 1 else 0))
  continue
executeInstruction (EQL)        = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush ((if (x == y) then 1 else 0))
  continue
executeInstruction (AND)        = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush ((if (x /= 0) && (y /= 0) then 1 else 0))
  continue
executeInstruction (OR)         = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush ((if (x /= 0) || (y /= 0) then 1 else 0))
  continue
executeInstruction (NOT)        = do
  state <- stState
  x     <- stackPop
  stackPush ((if (x == 0) then 1 else 0))
  continue
