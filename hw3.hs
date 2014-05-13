-- CS381 HW3
-- Team Members: Sean Rettig

module HW3 where

import Data.Maybe

-- Exercise 1
-- (a)
type Prog = [Cmd]
type Stack = [Int]
type D = Stack -> Maybe Stack
type Rank = Int
type CmdRank = (Int,Int)

data Cmd    = LD Int
            | ADD
            | MULT
            | DUP
            | INC
            | SWAP
            | POP Int
            deriving Show

rankC :: Cmd -> CmdRank
rankC (LD i)        = (0,1)
rankC (ADD)         = (2,1)
rankC (MULT)        = (2,1)
rankC (DUP)         = (1,2)
rankC (INC)         = (1,1)
rankC (SWAP)        = (2,2)
rankC (POP i)       = (i,0)

rank :: Prog -> Rank -> Maybe Rank
rank [] r           = Just r
rank (c:cs) r       | r >= fst rc && isJust rcs = Just $ fromJust $ rcs
                    | otherwise                 = Nothing
                    where   rc = rankC c
                            rcs = rank cs (r - fst rc + snd rc)

rankP :: Prog -> Maybe Rank
rankP p             = rank p 0

semCmd :: Cmd -> D
semCmd (LD i) s     = Just (i:s)
semCmd ADD (a:b:s)  = Just $ (a+b):s
semCmd MULT (a:b:s) = Just $ (a*b):s
semCmd DUP (a:s)    = Just $ a:a:s
semCmd INC (a:s)    = Just $ a+1:s
semCmd SWAP (a:b:s) = Just $ b:a:s
semCmd (POP 0) s    = Just $ s
semCmd (POP i) (_:s)= semCmd (POP (i-1)) s
semCmd _ _          = Nothing

sem :: Prog -> D
sem [] s            = Just s
sem (a:p) s         = case semCmd a s of
                        Just stack  -> sem p stack
                        Nothing     -> Nothing

-- (b)

initStack = []

semStatTC :: Prog -> Maybe Stack
semStatTC p         = case rankP p of
                        Just r      -> sem p initStack
                        otherwise   -> Nothing

-- The type of the sem function can be simplified if it is called from
-- semStatTC, because the type checking that is performed first ensures that a
-- Stack will be returned, rather than a Maybe Stack.  Therefore, its type may
-- be Prog -> Stack -> Stack.  Further, this simplifies its implementation as
-- well because semCmd will also return a Stack rather than a Maybe Stack, so
-- testing for the Nothing case is not necessary.

-- These tests can be run easily to verify the correctness of the solution.

-- Tests the semantics of all operations
testProg1 = [LD 3, DUP, MULT, LD 2, INC, LD 2, SWAP, ADD, LD 1, LD 1, POP 2]
testResult1 = semStatTC testProg1

-- Returns Nothing due to type error (popping two elements from a stack of one)
testProg2 = [LD 3, POP 2]
testResult2 = semStatTC testProg2
