module Day2 where

import Control.Lens

getOp :: Int -> (Int -> Int -> Int)
getOp 1 = (+) 
getOp 2 = (*) 
getOp _ = error "out of bounds"

runPgm :: [Int] -> [Int] -> [Int]
runPgm _ [] = []
runPgm state (opcode:inp1:inp2:out:mems)    
   | opcode == 99 = state
   | otherwise = runPgm newMem mems 
       where
           newMem = state & (element out) .~ (getOp opcode (state !! inp1) (state !! inp2))

search :: Int -> Int -> [Int] -> [Int]
search x y mem = 
        let 
            firstSetup = mem & (element 1) .~ x
            secondSetup = firstSetup & (element 2) .~ y
        in 
            runPgm secondSetup secondSetup

main :: IO ()
main = do 
    file <- readFile "d2"
    nums <- return . read $ "[" ++ file ++ "]" :: IO [Int]
    answer <- return $ head $ search 41 12 nums 
    print $ answer
