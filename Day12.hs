module Day12 where

import qualified Data.Text as T
import Text.Regex

type Coord = (Int,Int,Int)

combs :: Int -> [(Int,Int)]
combs x = [(x,y) | y <- [0..3], x/= y]

(>+<) :: Coord -> Coord -> Coord
(>+<) (a,b,c) (d,e,f) = (a+d, b+e, c+f)

nullV :: [Coord]
nullV = [(0,0,0),(0,0,0),(0,0,0),(0,0,0)]

testInp :: [Coord]
testInp = [(-1,0,2),(2,-10,-7),(4,-8,8),(3,5,-1)]

input :: [Coord]
input = [(6,10,10),(-9,3,17),(9,-4,14),(4,14,4)]

gravity :: Int -> Int -> Int
gravity n1 n2
    | n1 > n2 = -1
    | n1 == n2 = 0
    | n1 < n2 = 1
    
calcVel :: Coord -> Coord -> Coord
calcVel (a,b,c) (d,e,f) = (gravity a d, gravity b e, gravity c f)
    
velCoord :: [Coord] -> Int -> Coord
velCoord vs n = 
    foldl (>+<) (0,0,0) $ map (\(x,y) -> calcVel (vs !! x) (vs !! y)) (combs n)

motion :: [Coord] -> [Coord]
motion cs = map (velCoord cs) [0..3]

step :: Int -> [Coord] -> [Coord] -> ([Coord],[Coord])
step 0 ps vs = (ps,vs)
step n ps vs = 
    let
        newVel = (zipWith (>+<) vs (motion ps))
    in
        step (n-1) (zipWith (>+<) ps newVel) newVel

l1 :: Coord -> Int        
l1 (x,y,z) = abs x + abs y + abs z

energy :: ([Coord],[Coord]) -> Int
energy ([p1,p2,p3,p4],[v1,v2,v3,v4]) = sum [ l1 p1*l1 v1
                                           , l1 p2*l1 v2
                                           , l1 p3*l1 v3
                                           , l1 p4*l1 v4] 

main :: IO ()
main = do
    let k n = step n input nullV
        u n = (\(_,_,z) -> z) ((fst $ k n) !! 0)
    print $ map u [0..]
