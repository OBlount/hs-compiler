module TAMState where

import Data.List
import Control.Applicative
import Control.Monad

import ST (ST(..), stState, stUpdate)
import TAMCode (Instruction(..))
import MiniTriangle (Identifier)

type Stack = [Int]
type PC    = Int

data TAMState = TAMState {
  tsCode    :: [Instruction],
  tsCounter :: PC,
  tsStack   :: Stack
}

-- TAMState operations

stackPush :: Int -> ST TAMState ()
stackPush x = do
  state <- stState
  stUpdate (push x state)

stackPop :: ST TAMState Int
stackPop = do
  state <- stState
  let (x, state') = pop state
  stUpdate state'
  return x

stackSetCounter :: Int -> ST TAMState ()
stackSetCounter x = do
  state <- stState
  stUpdate (setCounter x state)

stackNextInstruction :: ST TAMState Instruction
stackNextInstruction = do
  state <- stState
  return (nextInstruction state)

getCounter :: ST TAMState PC
getCounter = do
  state <- stState
  return (tsCounter state)

updateCounter :: PC -> ST TAMState ()
updateCounter c = do
  state <- stState
  stUpdate (state { tsCounter = c })

continue :: ST TAMState ()
continue = do
  c <- getCounter
  updateCounter (c+1)

findLabel :: Identifier -> ST TAMState Int
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
