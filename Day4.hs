module Day4 where

import Text.Regex
import qualified Data.Text as T
import Data.List

infixr 8 >***<  
(>***<) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
(>***<) f g = \(a,b) -> (f a, g b)

triag :: a -> (a,(a,a))
triag a = (a,(a,a))

triagList :: (a,(a,a)) -> [a]
triagList (a1,(a2,a3)) = [a1,a2,a3]

diag :: a -> (a,a)
diag a = (a,a)
-- Comonadic behavior?
doubleFoldl :: (b -> a -> a -> b) -> b -> [a] -> b
doubleFoldl f b (a1:a2:as) = doubleFoldl f (f b a1 a2) (a2:as) 
doubleFoldl _ b _ = b

isIncreasing :: [Integer] -> Bool
isIncreasing = doubleFoldl (\b n1 n2 -> b && n2 <= n1) True

hasDoubles :: [Integer] -> Bool
hasDoubles = doubleFoldl (\b n1 n2 -> b || n2 == n1) False

validCluster :: [Integer] -> Bool
validCluster ns = 
      any (==2) 
    $ map (\n -> length . filter (==n) $ ns) [1 .. 9]

getDigits :: Integer -> [Integer]
getDigits n = 
    unfoldr (\x -> if x == 0 then 
            Nothing 
        else 
            Just . (mod x >***< div x) . diag $ 10) n

countPassword :: Integer -> Integer -> Int
countPassword x y = 
      length 
    $ filter (and . triagList . (validCluster >***< isIncreasing >***< hasDoubles) . triag) 
    $ map getDigits [x .. y]
--countPassword 387638 919123

    
