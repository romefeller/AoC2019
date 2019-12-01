module Day1 where

import Data.List (unfoldr)
infixr 8 >***<  
(>***<) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
(>***<) f g = \(a,b) -> (f a, g b)

fuels :: Integer -> Integer
fuels x = sum . tail $ unfoldr go x
    where 
        go fuel 
            | fromIntegral fuel <= 0 = Nothing 
            | otherwise = Just . (fromIntegral >***< mass . fromIntegral) $ (fuel,fuel)
            
mass :: Double -> Integer
mass x = (floor $ x/3.0) - 2

main :: IO ()
main = do 
    file <- readFile "d1"
    p <- return $ sum $ fmap (mass . read) $ words file :: IO Integer
    p2 <- return $ sum $ fmap (fuels . read) $ words file :: IO Integer
    putStrLn $ show (p,p2)
