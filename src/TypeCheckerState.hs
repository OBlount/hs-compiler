module TypeCheckerState where

import ST (ST(..), stState, stUpdate)
import MiniTriangle (Type(..), Identifier, TypeContext)

type ErrorMessage = String

data TypeCheckerState = TypeCheckerState {
  tcContext :: [TypeContext],
  tcErrors  :: [ErrorMessage]
}

run :: ST TypeCheckerState a -> a
run (S st) = fst $ st (TypeCheckerState { tcContext = [], tcErrors = [] })

-- TypeCheckerState operations

addVariableToContext :: TypeContext -> ST TypeCheckerState ()
addVariableToContext var = do
  state <- stState
  stUpdate (addVariable var state)

replaceVariableInContext :: TypeContext -> ST TypeCheckerState ()
replaceVariableInContext var = do
  state <- stState
  stUpdate (replaceVariable var state)

getContext :: ST TypeCheckerState [TypeContext]
getContext = do
  state <- stState
  return (tcContext state)

addError :: ErrorMessage -> ST TypeCheckerState ()
addError e = do
  state <- stState
  stUpdate (addErrorMessage e state)

getErrors :: ST TypeCheckerState [ErrorMessage]
getErrors = do
  state <- stState
  return (tcErrors state)

-- Auxiliary functions

addVariable :: TypeContext -> TypeCheckerState -> TypeCheckerState
addVariable var tcs = tcs { tcContext = (tcContext tcs) ++ [var] }

replaceVariable :: TypeContext -> TypeCheckerState -> TypeCheckerState
replaceVariable (id, t) tcs = tcs { tcContext = [(id', t') | (id', t') <- tcContext tcs, id' /= id] ++ [(id, t)] }

checkIfVariableExists :: Identifier -> [TypeContext] -> Bool
checkIfVariableExists id ctx = id `elem` map fst ctx

addErrorMessage :: ErrorMessage -> TypeCheckerState -> TypeCheckerState
addErrorMessage e tcs = tcs { tcErrors = (tcErrors tcs) ++ [e] }
