module TAMCode where

import MiniTriangle (Identifier)

data Instruction = LOAD Int
                 | STORE Int
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
  deriving (Show)
