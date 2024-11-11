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
        ".tam" -> do
                    let instructions = readInstructions contents
                    let initialState = TAMState { tsCode = instructions, tsCounter = 0, tsStack = [] }
                    execute executeProgram initialState
        _      -> error "Please provide a .mt or .tam file"
    []         -> error "Program usage: \n\t- ./mtc [filename].mt\n\t- ./mtc [filename].tam"

writeInstructions :: [Instruction] -> FilePath -> IO ()
writeInstructions is path = do
  let content = unlines (map show is)
  writeFile path content

readInstructions :: String -> [Instruction]
readInstructions input =
  case map readInstruction (lines input) of
    list -> [i | i <- list]

readInstruction :: String -> Instruction
readInstruction line = case words line of
  ["LOAD", n]    -> LOAD (read n)
  ["STORE", n]   -> STORE (read n)
  ["LOADL", n]   -> LOADL (read n)
  ["GETINT"]     -> GETINT
  ["PUTINT"]     -> PUTINT
  ["JUMP", l]    -> JUMP l
  ["JUMPIFZ", l] -> JUMPIFZ l
  ["Label", l]   -> Label l
  ["HALT"]       -> HALT
  ["ADD"]        -> ADD
  ["SUB"]        -> SUB
  ["MUL"]        -> MUL
  ["DIV"]        -> DIV
  ["LSS"]        -> LSS
  ["GRT"]        -> GRT
  ["EQL"]        -> EQL
  ["AND"]        -> AND
  ["OR"]         -> OR
  ["NOT"]        -> NOT
  _              -> error "[ERROR] - Something went wrong reading your file"
