-- Team Members: Sean Rettig

-- Exercise 1
-- (a)
data Cmd    = Pen Mode
            | Moveto (Pos, Pos)
            | Def String Pars Cmd
            | Call String Vals
            | Seq Cmd Cmd
            deriving Show

data Mode   = Up
            | Down
            deriving Show

data Pos    = Number Int
            | Name String
            deriving Show

data Pars   = Pars String Pars
            | Par String
            deriving Show

data Vals   = Vals Int Vals
            | Val Int
            deriving Show

-- (b)

-- Assignment makes no sense.  This is my best interpretation.
vector = Def "vector" (Pars "x1" (Pars "y1" (Pars "x2" (Par "y2")))) (Seq (Seq (Seq (Pen Up) (Moveto (Name "x1",Name "y1"))) (Pen Down)) (Moveto (Name "x2",Name "y2")))

-- (c)
--steps :: Int -> Cmd
--I gave up on this one because I don't see how it's possible, given the current information.

-- Exercise 2
-- (a)
data Circuit    = Circuit Gates Links
                deriving Show

data Gates      = EmptyGates
                | Gate (Int,GateFn) Gates
                deriving Show

data GateFn     = And
                | Or
                | Xor
                | Not
                deriving Show

data Links      = EmptyLinks
                | Link ((Int,Int),(Int,Int)) Links
                deriving Show

-- (b)
halfadder :: Circuit
halfadder = Circuit (Gate (1,Xor) (Gate (2,And) EmptyGates)) (Link ((1,1),(2,1)) (Link ((1,2),(2,2)) EmptyLinks))

-- (c)
ppCircuit :: Circuit -> String
ppCircuit (Circuit g l) = ppGates g++ppLinks l

ppGates :: Gates -> String
ppGates EmptyGates = ""
ppGates (Gate (i,f) g) = show i++":"++ppGateFn f++";\n"++ppGates g

ppGateFn :: GateFn -> String
ppGateFn And    = "and"
ppGateFn Or     = "or"
ppGateFn Xor    = "xor"
ppGateFn Not    = "not"

ppLinks :: Links -> String
ppLinks EmptyLinks = ""
ppLinks (Link ((a,b),(c,d)) g) = "from "++show a++"."++show b++" to "++show c++"."++show d++";\n"++ppLinks g

-- Exercise 3
data Expr   = N Int
            | Plus Expr Expr
            | Times Expr Expr
            | Neg Expr
            deriving Show

data Op     = Add | Multiply | Negate
            deriving Show

data Exp    = Num Int
            | Apply Op [Exp]
            deriving Show

-- (a)
myexp :: Exp
myexp = Apply Multiply [Apply Negate [Apply Add [Num 3,Num 4]],Num 7]

-- (b)
--
-- myexpr = Times (Neg (Plus (N 3) (N 4))) (N 7)
-- vs.
-- myexp = Apply Multiply [Apply Negate [Apply Add [Num 3,Num 4]],Num 7]
--
-- The Expr representation is more concise while still being readable,
-- but the Exp representation is more flexible; new operations can be added
-- by simply adding a new constructor name to the Op data type, without
-- needing to specify the arguments.  Additionally, since Apply takes a list
-- of numbers to operate on, the Exp function does not need to be modified to
-- apply operations with varying numbers of arguments.

-- (c)
translate :: Expr -> Exp
translate (N i)         = Num i
translate (Plus e1 e2)  = Apply Add $ map translate [e1,e2]
translate (Times e1 e2) = Apply Multiply $ map translate [e1,e2]
translate (Neg e1)      = Apply Negate $ map translate [e1]
