module Main where

import MiniTriangle
import Parser
import Compiler

main :: IO ()
main = do
  let src = "let var n; var x; in\
    \ begin getint(n); if n < 10 then x := 0 else x := 1; printint(x); end"
  putStrLn ("src> " ++ src)
  let parseResult = parse program src
  case parseResult of
    []          -> error "[ERROR] - Unable to parse program"
    [(ast, "")] -> print (compile ast)
