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
            | DEF String Prog
            | CALL String
            deriving (Read,Show)

semCmd :: Cmd -> D
semCmd (LD i) s     = Just (i:s)
semCmd ADD (a:b:s)  = Just $ (a+b):s
semCmd MULT (a:b:s) = Just $ (a*b):s
semCmd DUP (a:s)    = Just $ a:a:s
semCmd _ _          = Nothing

sem :: Prog -> D
sem [] s            = Just s
sem (a:p) s         = case semCmd a s of
                        Just stack  -> sem p stack
                        Nothing     -> Nothing

-- Exercise 2
-- (a) Done; see Cmd data type above.
-- (b)
type State = (Macros,Stack)
type Macros = [(String,Prog)]
type D2 = State -> Maybe State

-- (c)
semCmd2 :: Cmd -> D2
semCmd2 (LD i) (m,s)    = Just (m,i:s)
semCmd2 ADD (m,a:b:s)   = Just (m,(a+b):s)
semCmd2 MULT (m,a:b:s)  = Just (m,(a*b):s)
semCmd2 DUP (m,a:s)     = Just (m,a:a:s)
semCmd2 (DEF n p) (m,s) = Just ((n,p):m,s)
semCmd2 (CALL n) (m,s)  = case lookup n m of
                            Just p      -> sem2 p (m,s)
                            otherwise   -> sem2 [] (m,s)
semCmd2 _ _             = Nothing

sem2 :: Prog -> D2
sem2 [] (m,s)           = Just (m,s)
sem2 (a:p) (m,s)        = case semCmd2 a (m,s) of
                            Just state  -> sem2 p state
                            otherwise   -> Nothing

-- Exercise 3
data Cmd3   = Pen Mode
            | MoveTo Int Int
            | Seq Cmd3 Cmd3

data Mode   = Up | Down

type State3 = (Mode,Int,Int)
type Line = (Int,Int,Int,Int)
type Lines = [Line]

semS :: Cmd3 -> State3 -> (State3,Lines)
semS (Pen m') (m,x,y)       = ((m',x,y),[])
semS (MoveTo x' y') (m,x,y) = case m of
                                Up      -> (state,[])
                                Down    -> (state,[(x,y,x',y')])
                                where state = (m,x',y')
semS (Seq a b) s            = let   ra = semS a s
                                    rb = semS b (fst ra)
                              in    (fst rb,snd ra++snd rb)

sem' :: Cmd3 -> Lines
sem' c = snd $ semS c (Up,0,0)
