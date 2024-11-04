module Parser where

import Control.Applicative
import Data.Char

import MiniTriangle (Expr(..))
import MiniTriangle (BinaryOperator(..), UnaryOperator(..))
import MiniTriangle (Command(..), Declaration(..), Program(..), Identifier)

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

space :: Parser ()
space = P (\src -> let (ds, src') = span isSpace src
                   in [((), src')])

token :: Parser a -> Parser a
token pa = P (\src -> concat $
               map (\(_, src') -> parse pa src') (parse space src))

identifier :: Parser Identifier
identifier = some (sat isAlphaNum)

literalInt :: Parser Expr
literalInt =  token $ do
  xs <- some digit
  return (LiteralInt $ read xs)

-- Operator precedence where tier 1 is highest
tier8 :: Parser Expr
tier8 = do
  t  <- tier7
  ts <- many (do
      _   <- token (string "?")
      t'  <- tier7
      _   <- token (string ":")
      t'' <- tier7
      return (\base -> Conditional base t' t''))
  return (foldl (\base x -> x base) t ts)

tier7 :: Parser Expr
tier7 = do
  t  <- tier6
  ts <- many (do
      _  <- token (string "||")
      t' <- tier6
      return (\base -> BinOp Disjunction base t'))
  return (foldl (\base x -> x base) t ts)

tier6 :: Parser Expr
tier6 = do
  t  <- tier5
  ts <- many (do
      _  <- token (string "&&")
      t' <- tier5
      return (\base -> BinOp Conjunction base t'))
  return (foldl (\base x -> x base) t ts)

tier5 :: Parser Expr
tier5 = do
  not <- optional (token (string "!"))
  t   <- tier4
  return $ case not of
    Just _  -> UnOp Not t
    Nothing -> t

tier4 :: Parser Expr
tier4 = do
  t  <- tier3
  ts <- many (do
      _  <- token (string "<")
      t' <- tier3
      return (\base -> BinOp LessThan base t')
    <|> do
      _  <- token (string "<=")
      t' <- tier3
      return (\base -> BinOp LessThanEqual base t')
    <|> do
      _  <- token (string ">")
      t' <- tier3
      return (\base -> BinOp GreaterThan base t')
    <|> do
      _  <- token (string ">=")
      t' <- tier3
      return (\base -> BinOp GreaterThanEqual base t')
    <|> do
      _  <- token (string "==")
      t' <- tier3
      return (\base -> BinOp Equal base t')
    <|> do
      _  <- token (string "!=")
      t' <- tier3
      return (\base -> BinOp NotEqual base t'))
  return (foldl (\base x -> x base) t ts)

tier3 :: Parser Expr
tier3 = do
  t  <- tier2
  ts <- many (do
      _  <- token (string "+")
      t' <- tier2
      return (\base -> BinOp Addition base t')
    <|> do
      _  <- token (string "-")
      t' <- tier2
      return (\base -> BinOp Subtraction base t'))
  return (foldl (\base x -> x base) t ts)

tier2 :: Parser Expr
tier2 = do
  t  <- tier1
  ts <- many (do
      _  <- token (string "*")
      t' <- tier1
      return (\base -> BinOp Multiplication base t')
    <|> do
      _  <- token (string "/")
      t' <- tier1
      return (\base -> BinOp Division base t'))
  return (foldl (\base x -> x base) t ts)

tier1 :: Parser Expr
tier1 = do
  neg <- optional (token (string "-"))
  t   <- literalInt
  return $ case neg of
    Just _  -> UnOp Negation t
    Nothing -> t
