module HW2 where

import Prelude

-- var ::= (any variable name)
type Var = String

-- macro ::= (any macro name)
type Macro = String

-- prog ::= ε | cmd; prog
data Prog = Commands [Cmd]
    deriving (Eq, Show)

-- mode ::= down | up
data Mode = Down
          | Up
    deriving (Eq, Show)

-- expr ::= var | num | expr + expr
data Expr = Variable Var
          | Num Int
          | Add Expr Expr
    deriving (Eq, Show)

-- cmd ::= pen mode
--       | move(expr, expr)
--       | define macor(var*) { prog }
--       | call macro(expr*)
data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
    deriving (Eq, Show)

foo :: Expr
foo = Variable "fuck"

-- | Draws a line from (x1,y1) to (x2,y2)
--
-- define line (x1, y1, x2, y2) {
--     pen up; move(x1, y1);
--     pen down; move(x2, y2)
-- }
linePos1 :: Cmd
linePos1 = Move (Variable "x1") (Variable "y1")

linePos2 :: Cmd
linePos2 = Move (Variable "x2") (Variable "y2")

line :: Cmd
line = Define "line" ["x1", "y1", "x2", "y2"]
    (Commands [linePos1, linePos2])
