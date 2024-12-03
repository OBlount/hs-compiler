module TAMCode where

import MiniTriangle (Identifier)

data Address = Global Int
             | Local Int
  deriving (Show)

data Instruction = LOAD Address
                 | STORE Address
                 | LOADL Int
                 | GETINT
                 | PUTINT
                 | JUMP Identifier
                 | JUMPIFZ Identifier
                 | Label Identifier
                 | HALT
                 | ADD
                 | SUB
                 | MUL
                 | DIV
                 | LSS
                 | GRT
                 | EQL
                 | AND
                 | OR
                 | NOT
                 | CALL Identifier
                 | RETURN Int Int

instance Show Instruction where
  show (LOAD (Global a))  = "LOAD [SB + "  ++ (show $ abs a) ++ "]"
  show (LOAD (Local a))   = "LOAD [LB - "  ++ (show $ abs a) ++ "]"
  show (STORE (Global a)) = "STORE [SB + " ++ (show $ abs a) ++ "]"
  show (STORE (Local a))  = "STORE [LB - " ++ (show $ abs a) ++ "]"
  show (LOADL x)          = "LOADL "       ++ (show $ abs x)
  show GETINT             = "GETINT"
  show PUTINT             = "PUTINT"
  show (JUMP id)          = "JUMP "        ++ id
  show (JUMPIFZ id)       = "JUMPIFZ "     ++ id
  show (Label id)         = "Label "       ++ id
  show HALT               = "HALT"
  show ADD                = "ADD"
  show SUB                = "SUB"
  show MUL                = "MUL"
  show DIV                = "DIV"
  show LSS                = "LSS"
  show GRT                = "GRT"
  show EQL                = "EQL"
  show AND                = "AND"
  show OR                 = "OR"
  show NOT                = "NOT"
  show (CALL id)          = "CALL "        ++ id
  show (RETURN n m)       = "RETURN "      ++ (show $ abs n) ++ " " ++ (show $ abs m)
