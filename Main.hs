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
    []         -> error "Program usage: \n\t- ./mtc [filename].mt\n\t- ./mtc [filename.tam]"

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
  ["LOADL", n]     -> LOADL (read n)
  ["STORE", n]     -> STORE (read n)
  ["LOAD", n]      -> LOAD (read n)
  ["GETINT"]       -> GETINT
  ["PUTINT"]       -> PUTINT
  ["HALT"]         -> HALT
  ["ADD"]          -> ADD
  ["MUL"]          -> MUL
  ["LSS"]          -> LSS
  ["GRT"]          -> GRT
  ["NOT"]          -> NOT
  ["JUMP", l]      -> JUMP (removeQuotes l)
  ["JUMPIFZ", l]   -> JUMPIFZ (removeQuotes l)
  ["Label", l]     -> Label (removeQuotes l)
  _                -> error "[ERROR] - Something went wrong reading your file"

removeQuotes :: String -> String
removeQuotes str = if head str == '"' && last str == '"' then tail (init str)
                                                         else str
