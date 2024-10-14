module Parser where

import Scanner (Token(..), Operator(..))

data AST = LitInteger Integer
         | BinOp BinaryOperator AST AST
         | UnOp UnaryOperator AST
         deriving (Show)

data BinaryOperator = Addition | Subtraction | Multiplication | Division deriving (Show)

data UnaryOperator = Negation deriving (Show)

parse :: [Token] -> Maybe (AST, [Token])
parse = parseExpr

parseExpr :: [Token] -> Maybe (AST, [Token])
parseExpr ts = do
  (currentAST, rest) <- parseMExpr ts
  case rest of
    (Oper Plus : ts') -> do
      (nextAST, rest') <- parseExpr ts'
      Just (BinOp Addition currentAST nextAST, rest')
    (Oper Minus : ts') -> do
      (nextAST, rest') <- parseExpr ts'
      Just (BinOp Subtraction currentAST nextAST, rest')
    _ -> Just (currentAST, rest)

parseMExpr :: [Token] -> Maybe (AST, [Token])
parseMExpr ts = do
  (currentAST, rest) <- parseTerm ts
  case rest of
    (Oper Times : ts') -> do
      (nextAST, rest') <- parseMExpr ts'
      Just (BinOp Multiplication currentAST nextAST, rest')
    (Oper Divide : ts') -> do
      (nextAST, rest') <- parseMExpr ts'
      Just (BinOp Division currentAST nextAST, rest')
    _ -> Just (currentAST, rest)

parseTerm :: [Token] -> Maybe (AST, [Token])
parseTerm (Number x : ts) = Just (LitInteger $ toInteger x, ts)
parseTerm (OpenPar : ts)  = do
  (nextAST, rest) <- parseExpr ts
  case rest of
    (ClosedPar : rest') -> Just (nextAST, rest')
    _                   -> Nothing
parseFactor _ = Nothing
