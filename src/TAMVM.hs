module TAMVM where

import TAMState (TAMState(..), Stack, PC)
import TAMState ((!|!), continue, stackPush, stackPop, stackUpdate, findLabel, updateCounter)
import STIO (StateIO(..), stState, stUpdate, lift)
import TAMCode (Instruction(..))

executeProgram :: [Instruction] -> StateIO TAMState ()
executeProgram []     = return ()
executeProgram (i:is) = do
  execute i
  executeProgram (is)

execute :: Instruction -> StateIO TAMState ()
execute (LOAD a)      = do
  state <- stState
  let x = (tsStack state) !|! a
  stackPush x
  continue
execute (STORE addr)  = do
  x     <- stackPop
  state <- stState
  let updatedStack = stackUpdate (tsStack state) addr x
  stUpdate state { tsStack = updatedStack }
  continue
execute (LOADL x)    = do
  stackPush x
  continue
execute (GETINT)     = do
  lift (putStrLn "Enter a number: ")
  n <- lift getLine
  stackPush (read n)
  continue
execute (PUTINT)     = do
  n <- stackPop
  lift (putStrLn ("Output> " ++ (show n)))
  continue
execute (JUMP id)    = do
  state              <- stState
  instructionAddress <- findLabel id
  updateCounter instructionAddress
execute (JUMPIFZ id) = do
  state              <- stState
  instructionAddress <- findLabel id
  p                  <- stackPop
  if p == 0 then updateCounter instructionAddress
            else continue
execute (Label id)   = return ()
execute (HALT)       = return ()
execute (ADD)        = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush (x+y)
  continue
execute (SUB)        = do
  state <- stState
  y     <- stackPop
  x     <- stackPop
  stackPush (x-y)
  continue
execute (MUL)        = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush (x*y)
  continue
execute (DIV)        = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush (x `div` y)
  continue
execute (LSS)        = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush ((if (x < y) then 1 else 0))
  continue
execute (GRT)        = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush ((if (x > y) then 1 else 0))
  continue
execute (EQL)        = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush ((if (x == y) then 1 else 0))
  continue
execute (AND)        = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush ((if (x /= 0) && (y /= 0) then 1 else 0))
  continue
execute (OR)         = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush ((if (x /= 0) || (y /= 0) then 1 else 0))
  continue
execute (NOT)        = do
  state <- stState
  x     <- stackPop
  stackPush ((if (x == 0) then 1 else 0))
  continue
