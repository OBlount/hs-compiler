module Main where

import Scanner (scan)
import Parser (parse)
import Evaluator (eval)

main :: IO ()
main = do
  print $ eval $ parse $ scan "3 + 5"
