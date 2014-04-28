module HaskellPracticeTemplate where

import Data.List (sort)


type Bag a = [(a,Int)]


ins :: Eq a => a -> Bag a -> Bag a
ins v [] = [(v, 1)]
ins v ((a,b):xs) | a==v      = (a,b+1):xs
                 | otherwise = (a,b):ins v xs

del :: Eq a => a -> Bag a -> Bag a
del v [] = []
del v ((a,b):xs) | a==v && b>1 = (a,b-1):xs
                 | a==v        = xs
                 | otherwise   = (a,b):del v xs

bag :: Eq a => [a] -> Bag a
bag = foldr ins []
-- Valid but less fancy
--bag [] = []
--bag (x:xs) = ins x (bag xs)

subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] _ = True
subbag ((x,n):xs) ys = case lookup x ys of
                         Just m  -> n<=m && subbag xs ys
                         Nothing -> False
-- Valid but less fancy
--subbag ((b1,b2):xs) b' | f==[]     = False
--                       | otherwise = head f >= b2 && subbag xs b'
--                       where f = [n2 | (n1,n2) <- b', n1 == b1]

isbag :: Eq a => Bag a -> Bag a -> Bag a
isbag ((av,an):as) b = isbag as b ++ case lookup av b of
                        Just bn -> [(av,min an bn)]
                        Nothing -> []
isbag _ _ = []

size :: Bag a -> Int
size []         = 0
size ((_,n):xs) = n + size xs

hist :: Bag Int -> String
hist = undefined

ph :: Bag Int -> IO ()
ph = putStr . hist


xs,ys :: [Int]
xs = reverse [5,7,2,3,7,8,3,7]
ys = reverse [5,5,7,8,3,8,7]

lx = bag xs
ly = bag ys
lz = del 8 ly
la = del 5 lz
lb = del 3 la


type Number = Int

type Point = (Number,Number)
type Length = Number

data Shape = Pt Point
           | Circle Point Length
           | Rect Point Length Length
           deriving Show

type Figure = [Shape]

type BBox = (Point,Point)


width :: Shape -> Length
width = undefined

bbox :: Shape -> BBox
bbox = undefined

minX :: Shape -> Number
minX = undefined

move :: Shape -> Point -> Shape
move = undefined

alignLeft :: Figure -> Figure
alignLeft = undefined

inside :: Shape -> Shape -> Bool
inside = undefined

