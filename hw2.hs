-- Team Members: Sean Rettig

module HW2 where

-- Exercise 1
type Prog = [Cmd]
type Stack = [Int]
type D = Stack -> Maybe Stack

data Cmd    = LD Int
            | ADD
            | MULT
            | DUP

semCmd :: Cmd -> D
semCmd (LD i) s     = Just (i:s)
semCmd ADD (a:b:s)  = Just $ (a+b):s
semCmd MULT (a:b:s) = Just $ (a*b):s
semCmd DUP (a:s)    = Just $ a:a:s
semCmd _ _          = Nothing

sem :: Prog -> D
sem [] s            = Just s
--sem (a:p) s         = sem p (semCmd a s)
sem (a:p) s         = case semCmd a s of
                        Just stack  -> sem p stack
                        Nothing     -> Nothing
