module Main where

import System.Environment (getArgs)

import MiniTriangle
import Parser
import Compiler
import TAMVM
import TAMCode
import TAMState

main :: IO ()
main = do
  args <- getArgs
  case args of
    (fileName:_) -> do
      contents <- (readFile fileName)
      let name = fst (span (/= '.') fileName)
      let ext  = snd (span (/= '.') fileName)
      case ext of
        ".mt"  -> do
          case parse program contents of
            []                 -> error "[ERROR] - Unable to parse program"
            [(ast, endString)] -> do
              if endString == "" || endString == "\n" then writeInstructions (compile ast) (name ++ ".tam")
                                                      else error "[ERROR] - Unable to parse program"
        ".tam" -> undefined
        _      -> error "Please provide a .mt or .tam file"
    []         -> error "Program usage: \n\t- ./mtc [filename].mt\n\t- ./mtc [filename.tam]"

writeInstructions :: [Instruction] -> FilePath -> IO ()
writeInstructions is path = do
  let content = unlines (map show is)
  writeFile path content
