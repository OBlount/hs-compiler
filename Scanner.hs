module Scanner where

import Data.Char (isDigit, isSpace)

data Token = Number Int
           | Oper Operator
           | OpenPar | ClosedPar
           deriving (Show)

data Operator = Plus | Minus | Times | Divide deriving (Show)

scan :: String -> [Token]
scan src = case scanToken $ removeWhite src of
  Just (token, rest) -> token : scan rest
  Nothing            -> if (removeWhite src=="") then []
                                                 else error "Scanner: invalid token"

scanToken :: String -> Maybe (Token, String)
scanToken src = case scanInt src of
  Just (x, rest)    -> Just (Number x, rest)
  Nothing           -> case scanOp src of
    Just (op, rest) -> Just (Oper op, rest)
    Nothing         -> case scanOpenPar src of
      Just rest     -> Just (OpenPar, rest)
      Nothing       -> case scanClosedPar src of
       Just rest    -> Just (ClosedPar, rest)
       Nothing      -> Nothing

scanInt :: String -> Maybe (Int, String)
scanInt src = let (digits, rest) = span isDigit src
              in if digits == "" then Nothing
                                 else Just (read digits, rest)

scanOp :: String -> Maybe (Operator, String)
scanOp ('+':src) = Just (Plus, src)
scanOp ('-':src) = Just (Minus, src)
scanOp ('*':src) = Just (Times, src)
scanOp ('/':src) = Just (Divide, src)
scanOp _         = Nothing

scanOpenPar :: String -> Maybe String
scanOpenPar ('(':src) = Just src
scanOpenPar _         = Nothing

scanClosedPar :: String -> Maybe String
scanClosedPar (')':src) = Just src
scanClosedPar _         = Nothing

removeWhite :: String -> String
removeWhite src = dropWhile isSpace src

main :: IO ()
main = do
  putStrLn $ show $ scanInt "123 + 321"
  putStrLn $ show $ scanOp "+"
  putStrLn $ show $ scanOpenPar "("
  putStrLn $ show $ scanToken "999"
  putStrLn $ show $ scan "3    + 2"
