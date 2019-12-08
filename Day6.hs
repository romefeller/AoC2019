{-# LANGUAGE OverloadedStrings #-}
module Day6 where

import Data.List 
import qualified Data.Text as T
import qualified Data.Map as M

fromPath :: Ord a => [(a, a)] -> M.Map a [a]
fromPath = M.fromListWith (++) . map (fmap (:[]))

allPaths :: Ord a => M.Map a [a] -> a -> [[a]]
allPaths paths s =
    case M.lookup s paths of
        Nothing -> []                
        Just [] -> [[s]]  
        Just rs -> (s :) <$> (concat $ (allPaths paths) <$> rs) 

main :: IO ()
main = do 
    file <- readFile "testd6"
    p <- return $ fmap (T.splitOn ")" . T.pack) $ words file 
    paths <- return $ fromPath $ map (\(x:y:[]) -> (T.unpack x,T.unpack y)) p
    print paths
    print $ allPaths paths "K"
