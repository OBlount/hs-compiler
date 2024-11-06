module Compiler where

import Data.List

import MiniTriangle (Program(..), Declaration(..), Command(..), Identifier)
import MiniTriangle (Expr(..), BinaryOperator(..), UnaryOperator(..))
import TAMCode (Instruction(..), VarEnvironment)
import CompilerState (CompilerState(..), stUpdate, stState, getInstructions)

compile :: Program -> [Instruction]
compile (LetIn ds c) =
  let compiledCode = getInstructions (do
      vars <- declarationCode ds
      cmds <- commandCode c
      return (vars ++ cmds)) []
  in compiledCode ++ [HALT]

declarationCode :: [Declaration] -> CompilerState [Instruction]
declarationCode []                = return []
declarationCode (VarDecl id:ds)   = do
  env <- stState
  let addr = length env
  stUpdate ((id, addr) :env)
  rest <- declarationCode ds
  return (LOADL 0 : rest)
declarationCode (VarInit id v:ds) = do
  expr <- expressionCode v
  env  <- stState
  let addr = length env
  stUpdate ((id, addr) :env)
  rest <- declarationCode ds
  return (expr ++ rest)

commandCode :: Command -> CompilerState [Instruction]
commandCode (BeginEnd cs) = commandsCode cs

commandsCode :: [Command] -> CompilerState [Instruction]
commandsCode []                     = return []
commandsCode (Assign id e:cs)       = do
  expr <- expressionCode e
  env  <- stState
  rest <- commandsCode cs
  let addr = getAddress id env
  return (expr ++ [STORE addr] ++ rest)
commandsCode (IfThenElse e c c':cs) = undefined -- TODO
commandsCode (While e c:cs)         = undefined -- TODO
commandsCode (GetInt id:cs)         = do
  env  <- stState
  rest <- commandsCode cs
  let addr = getAddress id env
  return ([GETINT, STORE addr] ++ rest)
commandsCode (PrintInt e:cs)        = do
  expr <- expressionCode e
  rest <- commandsCode cs
  return (expr ++ [PUTINT] ++ rest)

expressionCode :: Expr -> CompilerState [Instruction]
expressionCode (LiteralInt x)   = return [LOADL x]
expressionCode (Var id)         = do
  env <- stState
  let addr = getAddress id env
  return [LOAD addr]
expressionCode (BinOp op e1 e2) = do
  expr1 <- expressionCode e1
  expr2 <- expressionCode e2
  return (expr1 ++ expr2 ++ binopCode op)
expressionCode (UnOp op e)      = do
  expr <- expressionCode e
  return (expr ++ unopCode op)

binopCode :: BinaryOperator -> [Instruction]
binopCode Addition         = [ADD]
binopCode Subtraction      = [SUB]
binopCode Multiplication   = [MUL]
binopCode Division         = [DIV]
binopCode LessThan         = [LSS]
binopCode LessThanEqual    = [] -- TODO: [LSS, JUMPIFZ nextCheck, LOADL1, JUMP end, Label nextCheck, EQL, Label end]
binopCode GreaterThan      = [GRT]
binopCode GreaterThanEqual = [] -- TODO
binopCode Equal            = [EQL]
binopCode NotEqual         = [EQL, NOT]
binopCode Conjunction      = [AND]
binopCode Disjunction      = [OR]

unopCode :: UnaryOperator -> [Instruction]
unopCode Negation = [NOT]
unopCode Not      = [NOT]

getAddress :: Identifier -> VarEnvironment -> Int
getAddress id env = case lookup id env of
  Just addr -> addr
  Nothing   -> error ("Variable " ++ id ++ " not found in env:\n" ++ show env)
