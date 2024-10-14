module Main where

import TAMVM (TAMInst(..))
import Scanner (scan)
import Parser (parse)
import CodeGenerator (expCode)

compArith :: String -> [TAMInst]
compArith src = expCode $ parse $ scan src

main :: IO ()
main = do
  let src = "3 * (3 * 5)"
  print $ compArith src
