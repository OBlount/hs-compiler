module TypeChecker where

import TypeCheckerState (TypeCheckerState(..), ErrorMessage)
import TypeCheckerState (run, addVariableToContext, getContext, addError, getErrors, checkIfVariableExists)
import ST (ST(..))
import MiniTriangle (Type(..), Expr(..), TypeContext)
import MiniTriangle (BinaryOperator(..), UnaryOperator(..))
import MiniTriangle (Declaration(..), Program(..))

typeCheck :: Program -> [ErrorMessage]
typeCheck (LetIn ds _) = run (do
        typeCheckDeclarations ds
        errors <- getErrors
        return errors)

typeCheckDeclarations :: [Declaration] -> ST TypeCheckerState ()
typeCheckDeclarations [] = return ()
typeCheckDeclarations ((VarDecl id t):ds) = do
  ctx <- getContext
  if checkIfVariableExists id ctx
  then addError ("Variable " ++ id ++ " is already declared.")
  else addVariableToContext (id, t)
  typeCheckDeclarations ds
typeCheckDeclarations ((VarInit id t e):ds) = do
  ctx <- getContext
  if checkIfVariableExists id ctx
  then addError $ "Variable " ++ id ++ " is already declared."
  else case typeCheckExpr ctx e of
    Nothing -> addError $ "Invalid type in initialization of " ++ id
    Just et ->
      if et /= t
      then addError $ "Type mismatch in initialization of " ++ id
      else addVariableToContext (id, t)
  typeCheckDeclarations ds
typeCheckDeclarations ((FunDecl _ _ _ _):ds) = typeCheckDeclarations ds -- TODO

typeCheckExpr :: [TypeContext] -> Expr -> Maybe Type
typeCheckExpr _ (LiteralInt _) = Just TInt
typeCheckExpr _ (LiteralBool _) = Just TBool
typeCheckExpr vc (Var id) = lookup id vc
typeCheckExpr vc (BinOp op e e') = do
  expr  <- typeCheckExpr vc e
  expr' <- typeCheckExpr vc e'
  binOpType op expr expr'
typeCheckExpr vc (UnOp op e) = do
  expr <- typeCheckExpr vc e
  unOpType op expr
typeCheckExpr vc (Conditional e e' e'') = do
  expr   <- typeCheckExpr vc e
  expr'  <- typeCheckExpr vc e'
  expr'' <- typeCheckExpr vc e''
  if expr == TBool && expr' == expr'' then Just expr' else Nothing
typeCheckExpr vc (Apply f as) = undefined -- TODO

binOpType :: BinaryOperator -> Type -> Type -> Maybe Type
binOpType Addition TInt TInt         = Just TInt
binOpType Subtraction TInt TInt      = Just TInt
binOpType Multiplication TInt TInt   = Just TInt
binOpType Division TInt TInt         = Just TInt
binOpType LessThan TInt TInt         = Just TBool
binOpType LessThanEqual TInt TInt    = Just TBool
binOpType GreaterThan TInt TInt      = Just TBool
binOpType GreaterThanEqual TInt TInt = Just TBool
binOpType Equal t t'
  | t == t'                          = Just TBool
binOpType NotEqual t t'
  | t == t'                          = Just TBool
binOpType Conjunction TBool TBool    = Just TBool
binOpType Disjunction TBool TBool    = Just TBool
binOpType _ _ _                      = Nothing

unOpType :: UnaryOperator -> Type -> Maybe Type
unOpType Negation TInt = Just TInt
unOpType Not TBool     = Just TBool
unOpType _ _           = Nothing

printAllErrors :: [ErrorMessage] -> IO ()
printAllErrors = mapM_ putStrLn
