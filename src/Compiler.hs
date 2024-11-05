module Compiler where

import MiniTriangle (Program(..), Declaration(..), Command(..))
import MiniTriangle (Expr(..), BinaryOperator(..), UnaryOperator(..))
import TAMCode (Instruction(..))

compile :: Program -> [Instruction]
compile (LetIn ds c) = (declarationCode ds) ++ (commandCode c) ++ [HALT]

programCode :: Program -> [Instruction]
programCode (LetIn ds c) = (declarationCode ds) ++ (commandCode c) ++ [HALT]

declarationCode :: [Declaration] -> [Instruction]
declarationCode []               = []
declarationCode (VarDecl _:ds)   = LOADL 0 : declarationCode ds
declarationCode (VarInit _ v:ds) = expressionCode v ++ declarationCode ds

commandCode :: Command -> [Instruction]
commandCode (BeginEnd cs) = commandsCode cs

commandsCode :: [Command] -> [Instruction]
commandsCode []                     = []
commandsCode (IfThenElse e c c':cs) = undefined -- TODO
commandsCode (While e c:cs)         = undefined -- TODO
commandsCode (GetInt l:cs)          = undefined -- TODO
commandsCode (PrintInt e:cs)        = expressionCode e ++ [PUTINT]

expressionCode :: Expr -> [Instruction]
expressionCode (LiteralInt x)         = [LOADL x]
expressionCode (Var id)               = [LOAD 0] -- TODO: load in identifier
expressionCode (BinOp op e e')        = expressionCode e ++ expressionCode e' ++ binopCode op
expressionCode (UnOp op e)            = expressionCode e ++ unopCode op
expressionCode (Conditional e e' e'') = undefined -- TODO

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
