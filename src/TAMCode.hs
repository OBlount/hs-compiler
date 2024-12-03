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
  deriving (Show)
