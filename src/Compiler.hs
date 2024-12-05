module Compiler where

import Control.Monad

import CompilerState (CompilerState(..), VarEnvironment)
import CompilerState (run, addVariableToEnv, getVarEnvironment, getVarCount, getFreshLabel, getAddressFromID, getLBFlag, setLBFlag)
import ST (ST(..))
import MiniTriangle (Program(..), Declaration(..), Command(..), Identifier)
import MiniTriangle (Expr(..), BinaryOperator(..), UnaryOperator(..))
import TAMCode (Instruction(..), Address(..))

compile :: Program -> [Instruction]
compile (LetIn ds c) = run (do
        setLBFlag False
        varCode <- declarationCode ds
        env     <- getVarEnvironment
        cmdCode <- commandCode env c
        setLBFlag True
        funCode <- functionDeclarationCode env ds
        return (varCode ++ cmdCode ++ [HALT] ++ funCode))

declarationCode :: [Declaration] -> ST CompilerState [Instruction]
declarationCode [] = return []
declarationCode ((VarDecl id _):ds) = do
  addr <- getVarCount
  addVariableToEnv [(id, addr)]
  rest <- declarationCode ds
  return (LOADL 0 : rest)
declarationCode ((VarInit id _ e):ds) = do
  addr <- getVarCount
  env  <- getVarEnvironment
  expr <- expressionCode env e
  addVariableToEnv [(id, addr)]
  rest <- declarationCode ds
  return (expr ++ rest)
declarationCode ((FunDecl _ _ _ _:ds)) = do declarationCode ds

commandCode :: VarEnvironment -> Command -> ST CompilerState [Instruction]
commandCode env (BeginEnd cs) = commandsCode env cs
commandCode env c             = commandsCode env [c]

commandsCode :: VarEnvironment -> [Command] -> ST CompilerState [Instruction]
commandsCode _ [] = return []
commandsCode env (Assign id e:cs) = do
  expr <- expressionCode env e
  flag <- getLBFlag
  let addr       = getAddressFromID id env
  let addressing = if flag then Local addr else Global addr
  rest <- commandsCode env cs
  return (expr ++ [STORE addressing] ++ rest)
commandsCode env (IfThenElse e c c':cs) = do
  expr      <- expressionCode env e
  thenCmd   <- commandsCode env [c]
  elseCmd   <- commandsCode env [c']
  elseLabel <- getFreshLabel
  endLabel  <- getFreshLabel
  rest      <- commandsCode env cs
  return (expr ++ [JUMPIFZ elseLabel] ++ thenCmd ++ [JUMP endLabel] ++ [Label elseLabel] ++ elseCmd ++ [Label endLabel] ++ rest)
commandsCode env (While e c:cs) = do
  expr       <- expressionCode env e
  body       <- commandCode env c
  startLabel <- getFreshLabel
  endLabel   <- getFreshLabel
  rest       <- commandsCode env cs
  return ([Label startLabel] ++ expr ++ [JUMPIFZ endLabel] ++ body ++ [JUMP startLabel] ++ [Label endLabel] ++ rest)
commandsCode env (GetInt id:cs) = do
  flag <- getLBFlag
  let addr       = getAddressFromID id env
  let addressing = if flag then Local addr else Global addr
  rest <- commandsCode env cs
  return ([GETINT, STORE addressing] ++ rest)
commandsCode env (PrintInt e:cs) = do
  expr <- expressionCode env e
  rest <- commandsCode env cs
  return (expr ++ [PUTINT] ++ rest)

expressionCode :: VarEnvironment -> Expr -> ST CompilerState [Instruction]
expressionCode env (LiteralInt x) = return [LOADL x]
expressionCode env (LiteralBool b) = if b then return [LOADL 1] else return [LOADL 0]
expressionCode env (Var id) = do
  flag <- getLBFlag
  let addr       = getAddressFromID id env
  let addressing = if flag then Local addr else Global addr
  return ([LOAD addressing])
expressionCode env (BinOp op e e') = do
  expr  <- expressionCode env e
  expr' <- expressionCode env e'
  return (expr ++ expr' ++ binopCode op)
expressionCode env (UnOp op e) = do
  expr <- expressionCode env e
  return (expr ++ unopCode op)
expressionCode env (Conditional e e' e'') = do
  expr      <- expressionCode env e
  expr'     <- expressionCode env e'
  expr''    <- expressionCode env e''
  elseLabel <- getFreshLabel
  endLabel  <- getFreshLabel
  return (expr ++ [JUMPIFZ elseLabel] ++ expr' ++ [JUMP endLabel] ++ [Label elseLabel] ++ expr'' ++ [Label endLabel])
expressionCode env (Apply f as) = do
  args <- mapM (expressionCode env) (reverse as)
  let args' = concat args
  return (args' ++ [CALL f])

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

functionDeclarationCode :: VarEnvironment -> [Declaration] -> ST CompilerState [Instruction]
functionDeclarationCode _ [] = return []
functionDeclarationCode env ((FunDecl id ps _ e):ds) = do
  let localEnv = zipWith (\(p, _) i -> (p, i)) ps (map negate [1..])
  let env'     = localEnv ++ env
  expr <- expressionCode env' e
  rest <- functionDeclarationCode env ds
  return ([Label id] ++ expr ++ [RETURN 1 (length ps)] ++ rest) -- Return will only return 1 value
functionDeclarationCode env (_:ds) = do functionDeclarationCode env ds
