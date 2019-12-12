module Main where

import qualified Data.Text as T
import Control.Monad

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

step :: Int -> [Coord] -> [Coord] -> IO ([Coord],[Coord])
step 0 ps vs = print (ps,vs) >> return (ps,vs)
step n ps vs = 
    let
        newVel = (zipWith (>+<) vs (motion ps))
        newPos = (zipWith (>+<) ps newVel)
    in
        (print $ (\(_,y,_) -> y) $ (newPos  !! 1)) >>
        step (n-1) newPos newVel

l1 :: Coord -> Int        
l1 (x,y,z) = abs x + abs y + abs z

energy :: ([Coord],[Coord]) -> Int
energy ([p1,p2,p3,p4],[v1,v2,v3,v4]) = sum [ l1 p1*l1 v1
                                           , l1 p2*l1 v2
                                           , l1 p3*l1 v3
                                           , l1 p4*l1 v4] 
--awk '{print NR-1 "," $1 }' y.txt | tr "\n" ";" | grep -on "[0-9]*,9;[0-9]*,7;[0-9]*,2;[0-9]*,-2;[0-9]*,-3;[0-9]*,-1"

-- x g
-- 231614,5;231615,4;231616,2;231617,1;231618,0;231619,-2
-- 463228,5;463229,4;463230,2;463231,1;463232,0;463233,-2
-- y g 
--116328,9;116329,7;116330,2;116331,-2;116332,-3;116333,-1
--232656,9;232657,7;232658,2;232659,-2;232660,-3;232661,-1
-- z g
--1:0,11;1,13;2,13;3,12;4,10;5,9
--102356,11;102357,13;102358,13;102359,12;102360,10;102361,9
main :: IO ()
main = do
    let k n = step n input nullV
    exec <- k 500000
    return ()
