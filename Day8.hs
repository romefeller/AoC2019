{-# LANGUAGE OverloadedStrings #-}
module Day8 where

import qualified Data.Text as T
import Data.List

imageLayers :: Int -> Int -> String -> [[String]]
imageLayers _ _ [] = []
imageLayers w t s = [map (\x -> take w (drop ((x-1)*w) s)) [1 .. t]] 
                 ++ imageLayers w t (drop (w*t) s)

fewestZero :: [String] -> Int
fewestZero ls = sum $ map (length . filter (=='0')) ls
        
visible :: String -> Char
visible = head . go . filter (/= '2') 
    where 
        go [] = ['2'] 
        go xs = xs

extract :: Int -> Int -> [[String]] -> Char
extract i j ls = visible $ map (!! i) $ map (!! j) ls 

main :: IO ()
main = do 
    file <- readFile "d8"
    img <- return . T.unpack . T.strip . T.pack $ file
    layers <- return $ imageLayers 25 6 img
    len <- return $ map fewestZero layers
    minLayer <- return . minimum $ len
    print $ layers !! ((\(Just x) -> x) $ findIndex (==minLayer) len)
    -- pt2
    vs <- return $ [extract i j layers | j <- [0 .. 5], i <- [0 .. 24]] 
    print $ imageLayers 25 6 vs
    
