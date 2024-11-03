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

expr :: Parser Expr
expr = do
  t  <- term
  ts <- many (do
      _  <- token (string "+")
      t' <- term
      return (\base -> BinOp Addition base t')
    <|> do
      _ <- token (string "-")
      t' <- term
      return (\base -> BinOp Subtraction base t'))
  return (foldl (\base x -> x base) t ts)

term :: Parser Expr
term = do
  x  <- literalInt
  xs <- many (do
      _  <- token (string "*")
      x' <- literalInt
      return (\base -> BinOp Multiplication base x')
    <|> do
      _  <- token (string "/")
      x' <- literalInt
      return (\base -> BinOp Division base x'))
  return (foldl (\base x -> x base) x xs)
