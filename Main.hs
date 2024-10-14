module Main where

import Data.Maybe (fromMaybe)

import Scanner (scan)
import Parser (parse)
import Evaluator (eval)

main :: IO ()
main = do
  print $ fromIntegral $ eval $ fst $ fromMaybe (error "Parsing failed") (parse $ scan "7 + (10/3) * 2")
