-- Class cs381 | Spring 2015
-- Author: Phillip Carter | 932-134-813

module HW3 where

import Prelude
import Data.List
import Data.Maybe


--
-- * Part 1: Simple Stack Language
--

--
-- ** Syntax
--

-- | A program is a list of commands.
type Prog = [Cmd]

-- | A command in the language manipulates the stack.
data Cmd = Push Int      -- push an int onto the stack
         | Pop           -- pop the top int off the stack
         | Dup           -- duplicate the int on top of the stack
         | Swap          -- swap the top two ints on the stack
         | Add           -- add the top two ints on the stack
  deriving (Eq,Show)


--
-- ** Semantics
--

-- | A stack of integers.
type Stack = [Int]


-- | Denotational semantics of a command.
--
--   >>> cmd (Push 4) [1,2,3]
--   Just [4,1,2,3]
--
--   >>> cmd Pop [1,2,3]
--   Just [2,3]
--
--   >>> cmd Dup [1,2,3]
--   Just [1,1,2,3]
--
--   >>> cmd Swap [4,5,6]
--   Just [5,4,6]
--
--   >>> cmd Add [4,5,6]
--   Just [9,6]
--
--   >>> cmd Pop []
--   Nothing
--
--   >>> cmd Dup []
--   Nothing
--
--   >>> cmd Swap []
--   Nothing
--
--   >>> cmd Swap [1]
--   Nothing
--
--   >>> cmd Add []
--   Nothing
--
--   >>> cmd Add [1]
--   Nothing
cmd :: Cmd -> Stack -> Maybe Stack
cmd (Push i) stack = Just(i : stack)
cmd (Pop) stack    = case stack of
	                 [] -> Nothing
	                 _  -> Just(tail stack)
cmd (Dup) stack    = case stack of
	                 [] -> Nothing
	                 _  -> Just((head stack) : stack)
cmd (Swap) stack   = case stack of
	                 []    -> Nothing
	                 [one] -> Nothing
	                 _     -> Just(second : first : theTail)
	                          where first   = head stack
	                                second  = head (tail stack)
	                                theTail = tail(tail stack)
cmd (Add) stack    = case stack of
	                 []    -> Nothing
	                 [one] -> Nothing
	                 _     -> Just((first + second) : theTail)
	                          where first   = head stack
	                                second  = head (tail stack)
	                                theTail = tail(tail stack)


-- | Denotational semantics of a program.
--
--   >>> prog [Push 3,Add] [5]
--   Just [8]
--
--   >>> prog [Dup,Pop] [5]
--   Just [5]
--
--   >>> prog [Dup,Add] [5]
--   Just [10]
--
--   >>> prog [Swap,Pop,Dup,Add] [5,3]
--   Just [10]
--
--   >>> prog [Pop,Dup] [5]
--   Nothing
progImpl :: Prog -> Maybe Stack -> Maybe Stack
progImpl [] s          = s
progImpl (c:p) Nothing = case c of
                         (Push i) -> progImpl p (cmd c [])
                         _        -> progImpl p Nothing
progImpl (c:p) s       = progImpl p (cmd c (fromJust s))

prog :: Prog -> Stack -> Maybe Stack
prog [] s    = Just s
prog p []    = progImpl p Nothing
prog p stack = progImpl p (Just stack)


-- | Evaluate a program starting with an empty stack.
run :: Prog -> Maybe Stack
run p = prog p []


--
-- * Part 2: Adding Macros
--

--
-- ** Syntax
--

-- | A macro name.
type Name = String

-- | Extended version of Prog that supports macros.
type XProg = [XCmd]

-- | Extended version of Cmd that supports macros.
data XCmd = Define Name XProg   -- define a macro
          | Call Name           -- call a macro
          | Basic Cmd           -- a basic command
  deriving (Eq,Show)

-- Some aliases for basic commands.
push n = Basic (Push n)
pop    = Basic Pop
dup    = Basic Dup
swap   = Basic Swap
add    = Basic Add

-- Some example macro definitions:

-- | A macro "double" that doubles the argument on top of the stack.
double :: XCmd
double = Define "double" [dup,add]

-- | A macro "triple" that triples the argument on top of the stack.
triple :: XCmd
triple = Define "triple" [dup,dup,add,add]

-- | A macro that uses "double" and "triple" (i.e. it assumes they
--   have already been defined).
trouble :: XCmd
trouble = Define "trouble" [dup, Call "triple", swap, Call "double"]


--
-- ** Semantics
--

-- | Associates macro names with their definitions.
type Macros = [(Name,XProg)]

-- | The runtime state of our extended stack language.
type State = (Macros,Stack)


-- | Semantics of an extended command.
xcmd :: XCmd -> State -> Maybe State
xcmd (Define name xp) (m, s) = Just ((name, xp) : m, s)
xcmd (Call name)      (m, s) = case result of
                               Just (_, xp)    -> xprog xp (m, s)
                               Nothing         -> Nothing
                               where result = find (\x -> fst x == name) m
xcmd (Basic c)        (m, s) = case result of
                               Just stack -> Just (m, stack)
                               Nothing    -> Just (m, [])
                               where result = cmd c s


-- | Semantics of an extended program.
xprogImpl :: XProg -> Maybe State -> Maybe State
xprogImpl [] state         = state
xprogImpl (xc:xcs) Nothing = xprogImpl xcs (xcmd xc ([], []))
xprogImpl (xc:xcs) state   = xprogImpl xcs (xcmd xc (fromJust state))


xprog :: XProg -> State -> Maybe State
xprog [] state = Just state
xprog xp state = xprogImpl xp (Just state)


-- | Evaluate a program starting with an empty stack.
--
--   >>> xrun [double,push 3,triple,Call "double"]
--   Just [6]
--
--   >>> xrun [double,push 3,triple,Call "triple"]
--   Just [9]
--
--   >>> xrun [double,push 3,triple,trouble,Call "trouble"]
--   Just [6,9]
--
--   >>> xrun [push 3,trouble,Call "trouble"]
--   Nothing
--
xrun :: XProg -> Maybe Stack
xrun xp = case state of
          Just (m, s) -> Just s
          Nothing     -> Nothing
          where state = xprog xp ([], [])
