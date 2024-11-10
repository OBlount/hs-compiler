module Compiler where

import CompilerState (CompilerState(..), VarEnvironment)
import CompilerState (run, addVariableToEnv, getVarEnvironment, getVarCount, getFreshLabel, getAddressFromID)
import ST (ST(..))
import MiniTriangle (Program(..), Declaration(..), Command(..), Identifier)
import MiniTriangle (Expr(..), BinaryOperator(..), UnaryOperator(..))
import TAMCode (Instruction(..))

compile :: Program -> [Instruction]
compile (LetIn ds c) = run (do
        varCode <- declarationCode ds
        env     <- getVarEnvironment
        cmdCode <- commandCode env c
        return (varCode ++ cmdCode ++ [HALT]))

declarationCode :: [Declaration] -> ST CompilerState [Instruction]
declarationCode []                = return []
declarationCode (VarDecl id:ds)   = do
  addr <- getVarCount
  addVariableToEnv [(id, addr)]
  rest <- declarationCode ds
  return (LOADL 0 : rest)
declarationCode (VarInit id e:ds) = do
  addr <- getVarCount
  env  <- getVarEnvironment
  let expr = expressionCode env e
  addVariableToEnv [(id, addr)]
  rest <- declarationCode ds
  return (expr ++ rest)

commandCode :: VarEnvironment -> Command -> ST CompilerState [Instruction]
commandCode env (BeginEnd cs) = commandsCode env cs
commandCode env c             = commandsCode env [c]

commandsCode :: VarEnvironment -> [Command] -> ST CompilerState [Instruction]
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
