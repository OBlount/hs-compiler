module Main where

import MiniTriangle
import Parser
import Compiler

main :: IO ()
main = do
  let src = "let var i := 3 in begin printint (i) end"
  putStrLn ("src> " ++ src)
  let parseResult = parse program src
  case parseResult of
    []          -> error "[ERROR] - Unable to parse program"
    [(ast, "")] -> print (compile ast)
