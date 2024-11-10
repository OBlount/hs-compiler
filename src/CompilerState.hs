module CompilerState where

import ST (ST(..), stState, stUpdate)
import MiniTriangle (Identifier)
import TAMCode (Instruction(..))

type VarEnvironment = [(Identifier, Int)]

data CompilerState = CompilerState {
  csVarEnv       :: VarEnvironment,
  csLabelCounter :: Int
}

run :: ST CompilerState [Instruction] -> [Instruction]
run (S st) = fst $ st (CompilerState { csVarEnv = [], csLabelCounter = 0 })

-- CompilerState operations

addVariableToEnv :: VarEnvironment -> ST CompilerState ()
addVariableToEnv var = do
  state <- stState
  stUpdate (addVariable var state)

getVarEnvironment :: ST CompilerState VarEnvironment
getVarEnvironment = do
  state <- stState
  return (csVarEnv state)

getVarCount :: ST CompilerState Int
getVarCount = do
  state <- stState
  return (length (csVarEnv state))

getFreshLabel :: ST CompilerState Identifier
getFreshLabel = do
  state <- stState
  let n = csLabelCounter state
  stUpdate (incrementLabelCounter state)
  return ('#':(show n))

-- Auxiliary functions

addVariable :: VarEnvironment -> CompilerState -> CompilerState
addVariable var cs = cs { csVarEnv = (csVarEnv cs) ++ var }

incrementLabelCounter :: CompilerState -> CompilerState
incrementLabelCounter cs = cs { csLabelCounter = (csLabelCounter cs) + 1 }

getAddressFromID :: Identifier -> VarEnvironment -> Int
getAddressFromID id env = case lookup id env of
  Just addr -> addr
  Nothing   -> error ("[ERROR] - Variable " ++ id ++ " not found in env:\n" ++ show env)
