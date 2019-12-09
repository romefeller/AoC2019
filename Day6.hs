{-# LANGUAGE OverloadedStrings #-}
module Day6 where

import Data.List 
import qualified Data.Text as T

allPaths :: Eq a => a -> [(a,a)] -> [[a]]
allPaths s paths = next
    where
      curr = filter ((s ==) . fst) paths
      ns = snd <$> curr
      next = case curr of
                  [] -> [[s]]
                  _ -> map (s:) $ concatMap (\x -> allPaths x paths) ns

main :: IO ()
main = do 
    file <- readFile "d6"
    p <- return $ fmap (T.splitOn ")" . T.pack) $ words file 
    paths <- return $ map (\(x:y:[]) -> (x,y)) p
    orbits <- return $ nub $ (map head p) ++ (map (!! 1) p)
    let numOrbits path x = length $ takeWhile (/= x) $ head $ filter (elem x) path
    print $ sum $ map (numOrbits (allPaths "COM" paths)) orbits
