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

step :: (Int,Int) -> Path -> [(Int,Int)]
step (x,y) (Path R a) = [(z,y) | z <- [x .. x+a]]
step (x,y) (Path L a) = [(z,y) | z <- [x-a .. x]]
step (x,y) (Path U a) = [(x,z) | z <- [y .. y+a]]
step (x,y) (Path D a) = [(x,z) | z <- [y-a .. y]]

findX :: [Path] -> [(Int,Int)]
findX ps = foldl (\c p -> (step (head c) p) ++ c ) [(0,0)] ps

main :: IO ()
main = do 
    --let r = mkRegex "\\[([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})\\] 
    --(falls asleep|wakes up|Guard #([0-9]+) begins shift)"
    --acts <- return $ . matchRegex r . T.unpack)
    file1 <- readFile "testd3"
    file2 <- readFile "test32"
    allPathsA <- return $ map (readP . T.unpack) $ T.splitOn (T.pack ",") (T.pack $ reverse $ tail $ reverse file1)
    allPathsB <- return $ map (readP . T.unpack) $ T.splitOn (T.pack ",") (T.pack $ reverse $ tail $ reverse file2)
    print $ (findX allPathsA) ++ findX allPathsB 
