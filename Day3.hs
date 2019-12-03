module Day3 where

import Text.Regex
import qualified Data.Text as T
import Data.List

data Dirs = R | U | D | L deriving (Read,Show)

data Path = Path{
    dir :: Dirs, 
    amt :: Int
} 

instance Show Path where 
    show (Path d a) = show d ++ show a    

readP :: String -> Path
readP (a:as) = Path (read [a]) (read as)

step :: (Int,Int) -> Path -> (Int,Int)
step (x,y) (Path R a) = (x+a,y)
step (x,y) (Path L a) = (x-a,y)
step (x,y) (Path U a) = (x,y+a)
step (x,y) (Path D a) = (x,y-a)

toCoord :: [Path] -> [(Int,Int)]
toCoord ps = foldl (\c p -> (step (head c) p) : c ) [(0,0)] ps

hasInter :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Bool
hasInter (x1,y1) (x2,y2) (x,y) = and [min x1 x2 <= x
                                     ,max x1 x2 >= x
                                     ,min y1 y2 <= y
                                     ,max y1 y2 >= y] 

points :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
points (a,b) (c,d) = [(x,y) | x<-[min a c .. max a c], y<-[min b d .. max b d]]

findInter :: [(Int,Int)] -> (Int,Int) -> (Int,Int) -> [(Int,Int)]
findInter (p1:p2:ps) q x = filter (hasInter q x) (points p1 p2) ++ findInter (p2:ps) q x
findInter _ _ _ = []

allInter :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
allInter pa (q:x:pb) = findInter pa q x ++ allInter pa (x:pb) 
allInter _ _ = []

main :: IO ()
main = do 
    file1 <- readFile "d3"
    file2 <- readFile "d32"
    allPathsA <- return $ map (readP . T.unpack) $ T.splitOn (T.pack ",") (T.pack $ reverse $ tail $ reverse file1)
    allPathsB <- return $ map (readP . T.unpack) $ T.splitOn (T.pack ",") (T.pack $ reverse $ tail $ reverse file2)
    inters <- return $ filter (/= (0,0)) $ allInter (toCoord allPathsA) (toCoord allPathsB)
    print inters
    print $ minimum $ map (\(x,y) -> abs x + abs y) inters
