module TAMVM where

import Control.Monad

import TAMState (TAMState(..), Stack, PC)
import TAMState ((!|!), continue, findLabel)
import TAMState (stackPush, stackPop, stackUpdate, stackNextInstruction, getStack, setStack)
import TAMState (updateCounter, getCounter)
import TAMState (getSB, getLB, updateLB, getStackPointer)
import STIO (StateIO(..), stState, stUpdate, lift)
import TAMCode (Instruction(..), Address(..))

execute :: StateIO TAMState a -> TAMState -> IO ()
execute (StT state) initialState = do
  (_, tamState) <- state initialState
  putStrLn ("Final stack: " ++ (show tamState))

executeProgram :: StateIO TAMState ()
executeProgram = do
  state <- stState
  i     <- stackNextInstruction
  case i of
    HALT -> return ()
    _    -> do
      executeInstruction i
      executeProgram

executeInstruction :: Instruction -> StateIO TAMState ()
executeInstruction (LOAD (Global offset)) = do
  state <- stState
  sb    <- getSB
  let a  = sb + offset
  let x  = (tsStack state) !|! a
  stackPush x
  continue
executeInstruction (LOAD (Local offset)) = do
  state <- stState
  lb    <- getLB
  let a  = lb - offset
  let x  = (tsStack state) !|! a
  stackPush x
  continue
executeInstruction (STORE (Global offset)) = do
  sb    <- getSB
  x     <- stackPop
  state <- stState
  let a            = sb + offset
  let updatedStack = stackUpdate (tsStack state) a x
  setStack updatedStack
  continue
executeInstruction (STORE (Local offset)) = do
  lb    <- getSB
  x     <- stackPop
  state <- stState
  let a            = lb - offset
  let updatedStack = stackUpdate (tsStack state) a x
  setStack updatedStack
  continue
executeInstruction (LOADL x) = do
  stackPush x
  continue
executeInstruction (GETINT) = do
  lift (putStrLn "Enter a number: ")
  n <- lift getLine
  stackPush (read n)
  continue
executeInstruction (PUTINT) = do
  n <- stackPop
  lift (putStrLn ("Output> " ++ (show n)))
  continue
executeInstruction (JUMP id) = do
  state              <- stState
  instructionAddress <- findLabel id
  updateCounter instructionAddress
executeInstruction (JUMPIFZ id) = do
  state              <- stState
  instructionAddress <- findLabel id
  p                  <- stackPop
  if p == 0 then updateCounter instructionAddress
            else continue
executeInstruction (Label id) = continue
executeInstruction (HALT) = return () -- Handled at higher level
executeInstruction (ADD) = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush (x+y)
  continue
executeInstruction (SUB) = do
  state <- stState
  y     <- stackPop
  x     <- stackPop
  stackPush (x-y)
  continue
executeInstruction (MUL) = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush (x*y)
  continue
executeInstruction (DIV) = do
  state <- stState
  y     <- stackPop
  x     <- stackPop
  stackPush (x `div` y)
  continue
executeInstruction (LSS) = do
  state <- stState
  y     <- stackPop
  x     <- stackPop
  stackPush (if (x < y) then 1 else 0)
  continue
executeInstruction (GRT) = do
  state <- stState
  y     <- stackPop
  x     <- stackPop
  stackPush (if (x > y) then 1 else 0)
  continue
executeInstruction (EQL) = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush (if (x == y) then 1 else 0)
  continue
executeInstruction (AND) = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush (if (x /= 0) && (y /= 0) then 1 else 0)
  continue
executeInstruction (OR) = do
  state <- stState
  x     <- stackPop
  y     <- stackPop
  stackPush (if (x /= 0) || (y /= 0) then 1 else 0)
  continue
executeInstruction (NOT) = do
  state <- stState
  x     <- stackPop
  stackPush (if (x == 0) then 1 else 0)
  continue
executeInstruction (CALL id) = do
  state <- stState
  a     <- findLabel id
  pc    <- getCounter
  lb    <- getLB
  stackPush lb
  sp    <- getStackPointer
  updateLB (sp-1)
  stackPush (pc)
  updateCounter a
  continue
executeInstruction (RETURN m n) = do
  vs     <- replicateM m stackPop
  a      <- stackPop
  prevLB <- stackPop
  replicateM_ n stackPop
  mapM_ stackPush (reverse vs)
  updateLB prevLB
  updateCounter a
  continue
