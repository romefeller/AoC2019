{-# LANGUAGE OverloadedStrings #-}
module Day6 where

import Data.List 
import qualified Data.Text as T

data RT a = RT a [RT a] deriving Show

instance Functor RT where
    fmap f (RT a rt) = RT (f a) (fmap (fmap f) rt)

add :: Eq a => [a] -> RT a -> RT a
add (x:y:_) (RT _ []) = RT y [RT x []]
add z@(x:y:[]) (RT a rt) 
    | x == a  = (RT y [(RT x rt)]) 
    | otherwise = RT a $ (add z) <$> rt
    
main :: IO ()
main = do 
    file <- readFile "testd6"
    p <- return $ fmap (T.splitOn ")" . T.pack) $ words file 
    print p
