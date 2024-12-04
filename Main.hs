module Main where

import System.Environment (getArgs)

import MiniTriangle
import Parser
import TypeChecker
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
        ".mt"  -> handleMT name contents
        ".tam" -> handleTAM name contents
        _      -> error "Please provide a .mt or .tam file"
    [] -> error "Program usage: \n\t- ./mtc [filename].mt\n\t- ./mtc [filename].tam"

handleMT :: String -> String -> IO ()
handleMT name contents = do
  let parsed = parse program contents
  case parsed of
    []                 -> error "[ERROR] - Unable to parse program"
    [(ast, endString)] -> do
      if endString == "" || endString == "\n"
      then do
        let errors = typeCheck ast
        if not $ null errors then do
          printAllErrors errors
          putStrLn "\n[ERROR] - Your program is not type safe. Exiting..."
        else do
          putStrLn "Program is type safe. Compiling..."
          let compiledProgram = compile ast
          writeInstructions compiledProgram (name ++ ".tam")
      else error "[ERROR] - Unable to parse program"

handleTAM :: String -> String -> IO ()
handleTAM name contents = do
  let instructions = readInstructions contents
  let initialState = TAMState { tsCode = instructions, tsCounter = 0, tsStack = [], tsSB = 0, tsLB = 0 }
  execute executeProgram initialState

writeInstructions :: [Instruction] -> FilePath -> IO ()
writeInstructions is path = do
  let content = unlines (map show is)
  writeFile path content

readInstructions :: String -> [Instruction]
readInstructions input = case parse tamInstructions [c | c <- input, c /= '"'] of
  [(program, endString)] -> if endString == "" || endString == "\n"
                            then program
                            else error "[ERROR] - Something went wrong reading your .tam file"
  [] -> error "[ERROR] - Something went wrong reading your .tam file"
