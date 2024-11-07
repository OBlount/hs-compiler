module Main where

import MiniTriangle
import Parser
import Compiler

main :: IO ()
main = do
  let src = "\
    \let var n;\
    \    var x;\
    \    var i \
    \in \
    \begin \
    \  getint (n);\
    \  if n < 0 then x := 0 else x := 1;\
    \  i := 2;\
    \  while i <= n do\
    \    begin\
    \      x := x * i;\
    \      i := i + 1\
    \    end;\
    \  printint (x)\
    \end"
  putStrLn ("src> " ++ src)
  let parseResult = parse program src
  case parseResult of
    []          -> error "[ERROR] - Unable to parse program"
    [(ast, "")] -> print (compile ast)
  putStrLn ("Success!")
