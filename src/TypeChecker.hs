module TypeChecker where

import Control.Monad

import TypeCheckerState (TypeCheckerState(..), ErrorMessage)
import TypeCheckerState (run, addVariableToContext, replaceVariableInContext, getContext, addError, getErrors, checkIfVariableExists)
import ST (ST(..))
import MiniTriangle (Type(..), Expr(..), TypeContext)
import MiniTriangle (BinaryOperator(..), UnaryOperator(..))
import MiniTriangle (Declaration(..), Command(..), Program(..))

typeCheck :: Program -> [ErrorMessage]
typeCheck (LetIn ds c) = run (do
        typeCheckDeclarations ds
        ctx <- getContext
        typeCheckCommand ctx c
        errors <- getErrors
        return errors)

typeCheckDeclarations :: [Declaration] -> ST TypeCheckerState ()
typeCheckDeclarations [] = return ()
typeCheckDeclarations ((VarDecl id t):ds) = do
  ctx <- getContext
  if checkIfVariableExists id ctx
  then do 
    replaceVariableInContext (id, t)
    addError ("Variable " ++ id ++ ": Identifier " ++ id ++ " declared more than once")
  else addVariableToContext (id, t)
  typeCheckDeclarations ds
typeCheckDeclarations ((VarInit id t e):ds) = do
  ctx <- getContext
  expr <- typeCheckExpr ctx e
  if checkIfVariableExists id ctx
  then case expr of
    Nothing -> do
      replaceVariableInContext (id, t)
      addError ("Variable " ++ id ++ ": Expression doesn't type-check")
      addError ("Variable " ++ id ++ ": Identifier " ++ id ++ " declared more than once")
    Just et ->
      if et /= t
      then do
        replaceVariableInContext (id, t)
        addError ("Variable " ++ id ++ ": Type of expression different from declared type")
        addError ("Variable " ++ id ++ ": Identifier " ++ id ++ " declared more than once")
      else do
        replaceVariableInContext (id, t)
        addError ("Variable " ++ id ++ ": Identifier " ++ id ++ " declared more than once")
  else case expr of
    Nothing -> do
      addVariableToContext (id, t)
      addError ("Variable " ++ id ++ ": Expression doesn't type-check")
    Just et ->
      if et /= t
      then do
        addVariableToContext (id, t)
        addError ("Variable " ++ id ++ ": Type of expression different from declared type")
      else addVariableToContext (id, t)
  typeCheckDeclarations ds
typeCheckDeclarations ((FunDecl id as t e):ds) = do
  ctx <- getContext
  expr <- typeCheckExpr (as ++ ctx ++ [(id, TFun (map snd as) t)]) e
  if checkIfVariableExists id ctx
  then case expr of
    Nothing -> do
      replaceVariableInContext (id, TFun (map snd as) t)
      addError ("Function " ++ id ++ ": Expression doesn't type-check")
      addError ("Function " ++ id ++ ": Identifier " ++ id ++ " declared more than once")
    Just et ->
      if et /= t
      then do
        replaceVariableInContext (id, TFun (map snd as) t)
        addError ("Function " ++ id ++ ": Type of expression different from declared type")
        addError ("Function " ++ id ++ ": Identifier " ++ id ++ " declared more than once")
      else do
        replaceVariableInContext (id, TFun (map snd as) t)
        addError ("Function " ++ id ++ ": Identifier " ++ id ++ " declared more than once")
  else case expr of
    Nothing -> do
      addVariableToContext (id, TFun (map snd as) t)
      addError ("Function " ++ id ++ ": Expression doesn't type-check")
    Just et ->
      if et /= t
      then do
        addVariableToContext (id, TFun (map snd as) t)
        addError ("Function " ++ id ++ ": Type of expression different from declared type")
      else addVariableToContext (id, TFun (map snd as) t)
  typeCheckDeclarations ds

typeCheckCommand :: [TypeContext] -> Command -> ST TypeCheckerState ()
typeCheckCommand ctx (BeginEnd cs) = typeCheckCommands ctx cs
typeCheckCommand ctx c             = typeCheckCommands ctx [c]

typeCheckCommands :: [TypeContext] -> [Command] -> ST TypeCheckerState ()
typeCheckCommands _ [] = return ()
typeCheckCommands ctx (Assign id e:cs) = do
  expr <- typeCheckExpr ctx e
  case lookup id ctx of
    Nothing -> addError ("assignment: " ++ id ++ " is not declared")
    Just t  -> case expr of
      Nothing -> addError ("assignment: Expression doesn't type-check for " ++ id)
      Just et -> 
        if et /= t
        then addError ("assignment: Expression has the wrong type for " ++ id)
        else return ()
  typeCheckCommands ctx cs
typeCheckCommands ctx (IfThenElse e c c':cs) = do
  expr <- typeCheckExpr ctx e
  let et = expr
  when (et /= Just TBool) (addError "if command: Condition is not a bool")
  typeCheckCommand ctx c
  typeCheckCommand ctx c'
  typeCheckCommands ctx cs
typeCheckCommands ctx (While e c:cs) = do
  expr <- typeCheckExpr ctx e
  let et = expr
  when (et /= Just TBool) (addError "while: Condition in While is not a bool")
  typeCheckCommand ctx c
  typeCheckCommands ctx cs
typeCheckCommands ctx (GetInt id:cs) = do
  case lookup id ctx of
    Just TInt -> return ()
    Just _    -> addError ("getint: Variable " ++ id ++ " must be an Integer")
    Nothing   -> addError ("getint: Variable " ++ id ++ " has not been declared")
  typeCheckCommands ctx cs
typeCheckCommands ctx (PrintInt e:cs) = do
  expr <- typeCheckExpr ctx e
  case expr of
    Nothing -> addError "printint: Expression doesn't type-check"
    Just t  -> when (t /= TInt) (addError "printint: You can only print integers")
  typeCheckCommands ctx cs

typeCheckExpr :: [TypeContext] -> Expr -> ST TypeCheckerState (Maybe Type)
typeCheckExpr _ (LiteralInt _) = return (Just TInt)
typeCheckExpr _ (LiteralBool _) = return (Just TBool)
typeCheckExpr ctx (Var id) = return (lookup id ctx)
typeCheckExpr ctx (BinOp op e e') = do
  expr  <- typeCheckExpr ctx e
  expr' <- typeCheckExpr ctx e'
  return $ case (expr, expr') of
    (Just t, Just t') -> binOpType op t t'
    _                 -> Nothing
typeCheckExpr ctx (UnOp op e) = do
  expr <- typeCheckExpr ctx e
  return $ case expr of
    Just t -> unOpType op t
    _      -> Nothing
typeCheckExpr ctx (Conditional e e' e'') = do
  expr   <- typeCheckExpr ctx e
  expr'  <- typeCheckExpr ctx e'
  expr'' <- typeCheckExpr ctx e''
  return $ case expr of
    Just TBool | expr' == expr'' -> expr'
    _                            -> Nothing
typeCheckExpr ctx (Apply id as) = do
  case lookup id ctx of
    Just (TFun as' t) -> do
      if length as /= length as'
      then do
        addError ("Function " ++ id ++ ": Expected " ++ (show (length as')) ++ " arguments, but got " ++ (show (length as)))
        return Nothing
      else do
        argumentTypes <- mapM (typeCheckExpr ctx) as
        if argumentTypes == map Just as'
        then return (Just t)
        else return Nothing
    _ -> return Nothing

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
