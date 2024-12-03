module Parser where

import Data.Char
import Data.List
import Control.Applicative
import Control.Monad

import MiniTriangle (Expr(..))
import MiniTriangle (BinaryOperator(..), UnaryOperator(..))
import MiniTriangle (Command(..), Declaration(..), Program(..), Identifier)
import TAMCode (Instruction(..), Address(..))

newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
  fmap f p = P (\src -> map (\(v, src') -> (f v, src')) (parse p src))

instance Applicative Parser where
  pure v    = P (\src -> [(v, src)])
  pg <*> px = P (\src -> concat $
                  map (\(g, src') -> parse (fmap g px) src') (parse pg src))

instance Monad Parser where
  p >>= f = P (\src -> concat $
                map (\(v, src') -> parse (f v) src') (parse p src))

instance Alternative Parser where
  empty   = P (\src -> [])
  p <|> q = P (\src -> case parse p src of
                []         -> parse q src
                [(v, out)] -> [(v, out)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) src = p src

item :: Parser Char
item = P (\src -> case src of
           [] -> []
           (c:src') -> [(c, src')])

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string []     = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)

whitespace :: Parser ()
whitespace = P (\src -> let (_, src') = span (\c -> isSpace c || c == ';' || c == '\n' || c == '\t') src
                   in [((), src')])

token :: Parser a -> Parser a
token pa = P (\src -> concat $
               map (\(_, src') -> parse pa src') (parse whitespace src))

identifier :: Parser Identifier
identifier = token $ do
  c  <- sat isAlpha <|> sat (\c -> c == '#')
  cs <- many (sat isAlphaNum)
  return (c:cs)

literalInt :: Parser Expr
literalInt =  token $ do
  xs <- some digit
  return (LiteralInt $ read xs)

integer :: Parser Int
integer = token $ do
  xs <- some digit
  return (read xs)

-- Parser for arithmetic and expressions (tier 1 = highest precedence)

parseExpr :: Parser Expr
parseExpr = tier8

tier8 :: Parser Expr
tier8 = do
  t  <- token tier7
  ts <- token $ many (do
      _   <- token (string "?")
      t'  <- token tier7
      _   <- token (string ":")
      t'' <- token tier7
      return (\base -> Conditional base t' t''))
  return (foldl (\base x -> x base) t ts)

tier7 :: Parser Expr
tier7 = do
  t  <- token tier6
  ts <- token $ many (do
      _  <- token (string "||")
      t' <- token tier6
      return (\base -> BinOp Disjunction base t'))
  return (foldl (\base x -> x base) t ts)

tier6 :: Parser Expr
tier6 = do
  t  <- token tier5
  ts <- token $ many (do
      _  <- token (string "&&")
      t' <- token tier5
      return (\base -> BinOp Conjunction base t'))
  return (foldl (\base x -> x base) t ts)

tier5 :: Parser Expr
tier5 = do
  not <- token $ optional (token (string "!"))
  t   <- tier4
  return $ case not of
    Just _  -> UnOp Not t
    Nothing -> t

tier4 :: Parser Expr
tier4 = do
  t  <- token tier3
  ts <- token $ many (do
      _  <- token (string "<")
      t' <- token tier3
      return (\base -> BinOp LessThan base t')
    <|> do
      _  <- token (string "<=")
      t' <- token tier3
      return (\base -> BinOp LessThanEqual base t')
    <|> do
      _  <- token (string ">")
      t' <- token tier3
      return (\base -> BinOp GreaterThan base t')
    <|> do
      _  <- token (string ">=")
      t' <- token tier3
      return (\base -> BinOp GreaterThanEqual base t')
    <|> do
      _  <- token (string "==")
      t' <- token tier3
      return (\base -> BinOp Equal base t')
    <|> do
      _  <- token (string "!=")
      t' <- token tier3
      return (\base -> BinOp NotEqual base t'))
  return (foldl (\base x -> x base) t ts)

tier3 :: Parser Expr
tier3 = do
  t  <- token tier2
  ts <- token $ many (do
      _  <- token (string "+")
      t' <- token tier2
      return (\base -> BinOp Addition base t')
    <|> do
      _  <- token (string "-")
      t' <- token tier2
      return (\base -> BinOp Subtraction base t'))
  return (foldl (\base x -> x base) t ts)

tier2 :: Parser Expr
tier2 = do
  t  <- token tier1
  ts <- token $ many (do
      _  <- token (string "*")
      t' <- token tier1
      return (\base -> BinOp Multiplication base t')
    <|> do
      _  <- token (string "/")
      t' <-token tier1
      return (\base -> BinOp Division base t'))
  return (foldl (\base x -> x base) t ts)

tier1 :: Parser Expr
tier1 = do
    _ <- token (string "-")
    t <- parseExpr
    return (UnOp Negation t)
  <|> do
    t <- token literalInt
    return (t)
  <|> do
    t <- token identifier
    return (Var t)
  <|> do
    _ <- token (string "(")
    e <- token parseExpr
    _ <- token (string ")")
    return (e)

-- Parser for commands

command :: Parser Command
command = token assign
      <|> token ifThenElse
      <|> token while
      <|> token getInt
      <|> token printInt
      <|> token beginEnd

assign :: Parser Command
assign = do
  id   <- token identifier
  _    <- token (string ":=")
  expr <- token parseExpr
  _    <- token whitespace
  return (Assign id expr)

ifThenElse :: Parser Command
ifThenElse = do
  _           <- token (string "if")
  condition   <- token parseExpr
  _           <- token (string "then")
  trueBranch  <- token command
  _           <- token (string "else")
  falseBranch <- token command
  _    <- token whitespace
  return (IfThenElse condition trueBranch falseBranch)

while :: Parser Command
while = do
  _         <- token (string "while")
  condition <- token parseExpr
  _         <- token (string "do")
  body      <- token command
  _    <- token whitespace
  return (While condition body)

getInt :: Parser Command
getInt = do
  _  <- token (string "getint")
  _  <- token (string "(")
  id <- token identifier
  _  <- token (string ")")
  _    <- token whitespace
  return (GetInt id)

printInt :: Parser Command
printInt = do
  _    <- token (string "printint")
  _    <- token (string "(")
  expr <- token parseExpr
  _    <- token (string ")")
  _    <- token whitespace
  return (PrintInt expr)

beginEnd :: Parser Command
beginEnd = do
  _    <- token (string "begin")
  cmds <- token (many command)
  _    <- token (string "end")
  _    <- token whitespace
  return (BeginEnd cmds)

-- Parser for declarations

declaration :: Parser Declaration
declaration = token declareInit
          <|> token declare

declareInit :: Parser Declaration
declareInit = do
  _    <- token (string "var")
  id   <- token identifier
  _    <- token (string ":=")
  expr <- token parseExpr
  return (VarInit id expr)

declare :: Parser Declaration
declare = do
  _  <- token (string "var")
  id <- token identifier
  return (VarDecl id)

-- Parser for programs

program :: Parser Program
program = do
  _    <- token (string "let")
  vars <- token (many declaration)
  _    <- token (string "in")
  body <- token command
  return (LetIn vars body)

-- Parser for .tam files

tamInstruction :: Parser Instruction
tamInstruction = (token (string "STORE")   >> token address >>= return . STORE)
             <|> (token (string "LOADL")   >> token integer >>= return . LOADL)
             <|> (token (string "LOAD")    >> token address >>= return . LOAD)
             <|> (token (string "GETINT")  >> return GETINT)
             <|> (token (string "PUTINT")  >> return PUTINT)
             <|> (token (string "JUMPIFZ") >> token identifier >>= return . JUMPIFZ)
             <|> (token (string "JUMP")    >> token identifier >>= return . JUMP)
             <|> (token (string "Label")   >> token identifier >>= return . Label)
             <|> (token (string "HALT")    >> return HALT)
             <|> (token (string "ADD")     >> return ADD)
             <|> (token (string "SUB")     >> return SUB)
             <|> (token (string "MUL")     >> return MUL)
             <|> (token (string "DIV")     >> return DIV)
             <|> (token (string "LSS")     >> return LSS)
             <|> (token (string "GRT")     >> return GRT)
             <|> (token (string "EQL")     >> return EQL)
             <|> (token (string "AND")     >> return AND)
             <|> (token (string "OR")      >> return OR)
             <|> (token (string "NOT")     >> return NOT)
             <|> (token (string "CALL")    >> token identifier >>= return . CALL)
             <|> (token (string "RETURN")  >> token integer >>= \n -> token integer >>= \m -> return (RETURN n m))

address :: Parser Address
address = token $ do
  _      <- token $ string "["
  base   <- token $ (string "SB" <|> string "LB")
  _      <- token $ (string "+" <|> string "-")
  offset <- token $ integer
  _      <- token $ string "]"
  case base of
    "SB" -> return (Global offset)
    "LB" -> return (Local offset)

tamInstructions :: Parser [Instruction]
tamInstructions = do
  is <- token $ many tamInstruction
  return (is)
