module Day10 where

import Data.List

testGrid :: [String]
testGrid =     [".#..#"
               ,"....."
               ,"#####"
               ,"....#"
               ,"...##"
               ]

type Coord = (Int,Int)

findAsteroidsInLine :: [String] -> Int -> [Coord]
findAsteroidsInLine grid j = 
    map (\ix -> (ix,j) ) $ findIndices (=='#') (grid !! j)

allAsteroids :: [String] -> [Coord]    
allAsteroids grid = 
    concatMap (findAsteroidsInLine grid) [0 .. length grid -1]
    
(>-<) :: Coord -> Coord -> Coord
(>-<) (x1,y1) (x2,y2) =(x2-x1,y2-y1)

-- Vetor form equation of a line
lineOfSight :: Coord -> Coord -> [Coord]
lineOfSight p1 p2 =
    let
        (dx,dy) = p1 >-< p2
        mdc = gcd dx dy
    in
        if mdc == 1 then 
            []
        else 
            [((fst p1) + (div dx mdc) * n, (snd p1) + (div dy mdc) * n) | n <- [1 .. (mdc-1)]] 

isVisible :: [String] -> Coord -> Coord -> Bool
isVisible grid p1 p2 = not $ any (=='#') inLine
                          
    where 
        inLine = map (\(x,y) -> grid !! y !! x) $ lineOfSight p1 p2 
    
countAst :: [String] -> Coord -> Int
countAst grid z = length . filter (== True) . map (isVisible grid z) . filter (/=z) . allAsteroids $ grid

maxCountAll :: [String] -> Int
maxCountAll grid = maximum $ map (countAst grid) $ allAsteroids grid

laser :: Coord -> Coord -> Double
laser p1 p2 = 
    let
        (dx,dy) = p2 >-< p1
        theta = atan2 (fromIntegral dx) (fromIntegral $ -dy)
    in
        theta + (if theta < 0 then 2*pi else 0)
     

main :: IO ()
main = do 
    file <- readFile "d10"
    print $ maxCountAll $ words file
