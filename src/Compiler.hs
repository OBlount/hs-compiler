module Compiler where

import Data.List

import MiniTriangle (Program(..), Declaration(..), Command(..), Identifier)
import MiniTriangle (Expr(..), BinaryOperator(..), UnaryOperator(..))
import TAMCode (Instruction(..))
import ST (ST(..), stState, stUpdate)

type VarEnvironment = [(Identifier, Int)]

compile :: Program -> [Instruction]
compile (LetIn ds c) =
  let (varCode, varEnv) = getInstructionsAndState (declarationCode ds) []
      (cmdCode, _)      = getInstructionsAndState (commandCode varEnv c) 0
  in varCode ++ cmdCode ++ [HALT]

declarationCode :: [Declaration] -> ST VarEnvironment [Instruction]
declarationCode []                = return []
declarationCode (VarDecl id:ds)   = do
  env <- stState
  let addr = length env
  stUpdate ((id, addr) :env)
  rest <- declarationCode ds
  return (LOADL 0 : rest)
declarationCode (VarInit id e:ds) = do
  env <- stState
  let expr = expressionCode env e
  let addr = length env
  stUpdate ((id, addr) :env)
  rest <- declarationCode ds
  return (expr ++ rest)

commandCode :: VarEnvironment -> Command -> ST Int [Instruction]
commandCode env (BeginEnd cs) = commandsCode env cs
commandCode _ _               = error "[ERROR] - Unable to compile commands of your program"

commandsCode :: VarEnvironment -> [Command] -> ST Int [Instruction]
commandsCode _ []                       = return []
commandsCode env (Assign id e:cs)       = do
  let expr = expressionCode env e
  let addr = getAddressFromID id env
  rest <- commandsCode env cs
  return (expr ++ [STORE addr] ++ rest)
commandsCode env (IfThenElse e c c':cs) = do
  let expr = expressionCode env e
  thenCmd   <- commandsCode env [c]
  elseCmd   <- commandsCode env [c']
  elseLabel <- getFreshLabel
  endLabel  <- getFreshLabel
  rest      <- commandsCode env cs
  return (expr ++ [JUMPIFZ elseLabel] ++ thenCmd ++ [JUMP endLabel] ++ [Label elseLabel] ++ elseCmd ++ [Label endLabel] ++ rest)
commandsCode env (While e c:cs)         = do
  let expr = expressionCode env e
  body       <- commandCode env c
  startLabel <- getFreshLabel
  endLabel   <- getFreshLabel
  rest       <- commandsCode env cs
  return ([Label startLabel] ++ expr ++ [JUMPIFZ endLabel] ++ body ++ [JUMP startLabel] ++ [Label endLabel] ++ rest)
commandsCode env (GetInt id:cs)         = do
  let addr = getAddressFromID id env
  rest <- commandsCode env cs
  return ([GETINT, STORE addr] ++ rest)
commandsCode env (PrintInt e:cs)        = do
  let expr = expressionCode env e
  rest <- commandsCode env cs
  return (expr ++ [PUTINT] ++ rest)

expressionCode :: VarEnvironment -> Expr -> [Instruction]
expressionCode env (LiteralInt x)         = [LOADL x]
expressionCode env (Var id)               = let addr = getAddressFromID id env in [LOAD addr]
expressionCode env (BinOp op e e')        = (expr ++ expr' ++ binopCode op)
  where
    expr  = expressionCode env e
    expr' = expressionCode env e'
expressionCode env (UnOp op e)            = (expr ++ unopCode op)
  where
    expr = expressionCode env e
expressionCode env (Conditional e e' e'') = undefined -- TODO

binopCode :: BinaryOperator -> [Instruction]
binopCode Addition         = [ADD]
binopCode Subtraction      = [SUB]
binopCode Multiplication   = [MUL]
binopCode Division         = [DIV]
binopCode LessThan         = [LSS]
binopCode LessThanEqual    = [GRT, NOT]
binopCode GreaterThan      = [GRT]
binopCode GreaterThanEqual = [LSS, NOT]
binopCode Equal            = [EQL]
binopCode NotEqual         = [EQL, NOT]
binopCode Conjunction      = [AND]
binopCode Disjunction      = [OR]

unopCode :: UnaryOperator -> [Instruction]
unopCode Negation = [NOT]
unopCode Not      = [NOT]

-- Compiler state operations

getInstructionsAndState :: ST s a -> s -> (a, s)
getInstructionsAndState (S st) env = st env

getFreshLabel :: ST Int Identifier
getFreshLabel = do
  n <- stState
  stUpdate (n+1)
  return ('#':(show n))

getAddressFromID :: Identifier -> VarEnvironment -> Int
getAddressFromID id env = case lookup id env of
  Just addr -> addr
  Nothing   -> error ("[ERROR] - Variable " ++ id ++ " not found in env:\n" ++ show env)
