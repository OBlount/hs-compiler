module Main where

import System.Environment (getArgs)

import TAMVM (executeTAM, TAMInst(..))
import Scanner (scan)
import Parser (parse)
import CodeGenerator (expCode)

compArith :: String -> [TAMInst]
compArith src = expCode $ parse $ scan src

main :: IO ()
main = do
  args <- getArgs
  case args of
    (fileName:_) -> do
      file <- readFile $ fileName
      let ext = snd $ span (/= '.') fileName
      case ext of
        ".exp" -> writeFile ((fst $ span (/= '.') fileName) ++ ".tam") $ show $ compArith $ filter (/= '\n') file
        ".tam" -> error "Not implemented yet"
        _      -> error "Please provide either a .exp or a .tam file"
    [] -> do
      error "Please provide a file"
