module TAMState where

import Data.List

import STIO (StateIO(..), stState, stUpdate)
import TAMCode (Instruction(..))
import MiniTriangle (Identifier)

type Stack = [Int]
type PC    = Int

data TAMState = TAMState {
  tsCode    :: [Instruction],
  tsCounter :: PC,
  tsStack   :: Stack,
  tsSB      :: Int,
  tsLB      :: Int
}

instance Show TAMState where
  show (TAMState _ _ stack _ _) = (show stack)

-- TAMState operations

stackPush :: Int -> StateIO TAMState ()
stackPush x = do
  state <- stState
  stUpdate (push x state)

stackPop :: StateIO TAMState Int
stackPop = do
  state <- stState
  let (x, state') = pop state
  stUpdate state'
  return x

stackSetCounter :: Int -> StateIO TAMState ()
stackSetCounter x = do
  state <- stState
  stUpdate (setCounter x state)

stackNextInstruction :: StateIO TAMState Instruction
stackNextInstruction = do
  state <- stState
  return (nextInstruction state)

getStack :: StateIO TAMState Stack
getStack = do
  state <- stState
  return (tsStack state)

setStack :: Stack -> StateIO TAMState ()
setStack s = do
  state <- stState
  stUpdate (state { tsStack = s })

getStackPointer :: StateIO TAMState Int
getStackPointer = do
  state <- stState
  return (length (tsStack state))

getCounter :: StateIO TAMState PC
getCounter = do
  state <- stState
  return (tsCounter state)

updateCounter :: PC -> StateIO TAMState ()
updateCounter c = do
  state <- stState
  stUpdate (state { tsCounter = c })

getSB :: StateIO TAMState Int
getSB = do
  state <- stState
  return (tsSB state)

getLB :: StateIO TAMState Int
getLB = do
  state <- stState
  return (tsLB state)

updateLB :: Int -> StateIO TAMState ()
updateLB lb = do
  state <- stState
  stUpdate (state { tsLB = lb })

continue :: StateIO TAMState ()
continue = do
  c <- getCounter
  updateCounter (c+1)

findLabel :: Identifier -> StateIO TAMState Int
findLabel id = do
  state <- stState
  let is    = tsCode state
  let index = findIndex isLabelWithID is
  case index of
    Nothing -> error ("[ERROR] - No such label '" ++ id ++ "' found in memory")
    Just n  -> return n
  where
    isLabelWithID (Label l) = l == id
    isLabelWithID _         = False

-- Auxiliary functions

push :: Int -> TAMState -> TAMState
push x ts = ts {tsStack = x : (tsStack ts)}

pop :: TAMState -> (Int, TAMState)
pop ts = let (x:xs) = tsStack ts in (x,ts { tsStack = xs })

setCounter :: Int -> TAMState -> TAMState
setCounter i ts = ts { tsCounter = i }

nextInstruction :: TAMState -> Instruction
nextInstruction ts = (tsCode ts) !! (tsCounter ts)

stackUpdate :: Stack -> Int -> Int -> Stack
stackUpdate stack addr value = reverse (take addr (reverse stack) ++ [value] ++ drop (addr+1) (reverse stack))

(!|!) :: Stack -> Int -> Int
stack !|! addr = if addr < (length stack)
                   then (reverse stack) !! addr 
                   else error "[ERROR] - Indexed stack out of bounds"
