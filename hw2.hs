-- Class cs381 | Spring 2015
-- Author: Phillip Carter | 932-134-813

module HW2 where

import Prelude
import Data.List

-- macro ::= (any macro name)
type Macro = String

-- prog ::= ε | cmd; prog
type Prog = [Cmd]

-- mode ::= down | up
data Mode = Down
          | Up
    deriving (Eq, Show)

-- expr ::= var | num | expr + expr
data Expr = Var String
          | Num Int
          | Add Expr Expr
    deriving (Eq, Show)

-- cmd ::= pen mode
--       | move(expr, expr)
--       | define macor(var*) { prog }
--       | call macro(expr*)
data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [String] Prog
         | Call Macro [Expr]
    deriving (Eq, Show)

-- | Draw a line from (x1,y1) to (x2,y2)
--
-- define line (x1, y1, x2, y2) {
--     pen up; move(x1, y1);
--     pen down; move(x2, y2)
-- }
linePos1 :: Cmd
linePos1 = Move (Var "x1") (Var "y1")

linePos2 :: Cmd
linePos2 = Move (Var "x2") (Var "y2")

line :: Cmd
line = Define "line" ["x1", "y1", "x2", "y2"]
    [Pen Up, linePos1, Pen Down, linePos2]

-- | Use line macro to define MiniLogo macro nix (x,y,w,h)
--
-- define nix (x,y,w,h) {
--    pen up; move(x,y);
--    pen down; move (x + w/2, y + h/2);
--    pen up; move(x,y);
--    pen down; move (x - x/2, y + h/2);
--    pen up; move(x,y);
--    pen down; move (x - x/2, y - h/2);
--    pen up; move(x,y);
--    pen down; move (x + x/2, y - h/2)
-- }
centerPos :: Cmd
centerPos = Move (Var "x") (Var "y")

center :: [Cmd]
center = [Pen Up, centerPos]

topRightCornerPos :: Cmd
topRightCornerPos = Move (Add (Var "x") (Var "w/2")) (Add (Var "y") (Var "y/2"))

topRightCorner :: [Cmd]
topRightCorner = center ++ [Pen Down, topRightCornerPos]

topLeftCornerPos :: Cmd
topLeftCornerPos = Move (Add (Var "x") (Var "-w/2")) (Add (Var "y") (Var "y/2"))

topLeftCorner :: [Cmd]
topLeftCorner = center ++ [Pen Down, topLeftCornerPos]

bottomLeftCornerPos :: Cmd
bottomLeftCornerPos = Move (Add (Var "x") (Var "-w/2")) (Add (Var "y") (Var "-y/2"))

bottomLeftCorner :: [Cmd]
bottomLeftCorner = center ++ [Pen Down, bottomLeftCornerPos]

bottomRightCornerPos :: Cmd
bottomRightCornerPos = Move (Add (Var "x") (Var "w/2")) (Add (Var "y") (Var "-y/2"))

bottomRightCorner :: [Cmd]
bottomRightCorner = center ++ [Pen Down, bottomRightCornerPos]

nix :: Cmd
nix = Define "nix" ["x", "y", "w", "h"]
    (concat [topRightCorner, topLeftCorner, bottomLeftCorner, bottomRightCorner])

-- | Define haskell function steps which produces a staircase from (0,0)
--
-- The gist here is to move up 1 and move right 1 n number of times.
stepsImpl :: Int -> [Cmd]
stepsImpl 0 = [Pen Up, Move (Num 0) (Num 0), Pen Down]
stepsImpl n = (stepsImpl (n-1)) ++ [Move (Num 1) (Num 0), Move (Num 0) (Num 1)]

steps :: Int -> Prog
steps n = case n of
          0 -> [Pen Up, Move (Num 0) (Num 0)]
          _ -> stepsImpl n

-- | Define Haskell function macros which returns a list of used macro names
--
-- Given a program, find the names of macros and return those as a list.
-- Duplicates are not a concern; feel free to include them.
isMacro :: Cmd -> Bool
isMacro cmd = case cmd of
              (Define _ _ _) -> True
              _ -> False

toMacro :: Cmd -> Macro
toMacro (Define macroName _ _) = macroName

macros :: Prog -> [Macro]
macros = map toMacro . filter isMacro

-- | Pretty-print a MiniLogo program.
--
-- Given a Program, transforms the abstract syntax into well-formed concrete syntax.
prettyPen :: Mode -> String
prettyPen mode = case mode of
                 Up -> "pen up"
                 Down -> "pen down"

prettyExpr :: Expr -> String
prettyExpr expr = case expr of
                  (Var var) -> var
                  (Num num) -> show num
                  (Add expr1 expr2) -> (prettyExpr expr1) ++ " + " ++ (prettyExpr expr2)

prettyMove :: Expr -> Expr -> String
prettyMove expr1 expr2 = "move(" ++ (prettyExpr expr1) ++ "," ++ (prettyExpr expr2) ++ ")"

toPrettyString :: Cmd -> String
toPrettyString cmd = case cmd of
                     (Pen mode) -> prettyPen mode
                     (Move expr1 expr2) -> prettyMove expr1 expr2
                     (Define macro vars prog) -> "define " ++ macro ++ " (" ++ ((concat . intersperse ", ") vars) ++  ") " ++ " {\n" ++ (pretty prog) ++ "\n}"

pretty :: Prog -> String
pretty = concat . intersperse "; \n" . map toPrettyString
