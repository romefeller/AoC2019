module Day5 where

import Text.Regex
import qualified Data.Text as T
import Data.List

import Control.Lens 

leadZero opc = getDigits opc ++[0,0,0,0]

type State = [Int]

infixr 8 >***<  
(>***<) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
(>***<) f g = \(a,b) -> (f a, g b)

getDigits :: Int -> [Int]
getDigits n = 
    unfoldr (\x -> if x == 0 then 
            Nothing 
        else 
            Just . (mod x >***< div x) $ (10,10)) n

getOp :: Int -> (Int -> Int -> Int)
getOp 1 = (+) 
getOp 2 = (*) 
getOp x = error $ show x

-- pt1 -> 1, pt2 -> 5
runPgm :: Int -> State -> IO State  
runPgm ptr state = 
    let 
        opcode = state !! ptr
        inp1 = state   !! (ptr+1)
        inp2 = state   !! (ptr+2)
        out =  state   !! (ptr+3)
        c = if leadZero opcode !! 2 == 1 then inp1 else (state !! inp1)
        b = if leadZero opcode !! 3 == 1 then inp2 else (state !! inp2)
        a = out --if leadZero opcode !! 4 == 1 then out else (state !! out
    in
        case (mod opcode 100) of
            3 -> runPgm (ptr+2) (state & (element $ inp1) .~ 5)
            4 -> print (state !! inp1) >> runPgm (ptr+2) state
            5 -> if c /= 0 then runPgm b state 
                           else runPgm (ptr+3) state 
            6 -> if c == 0 then runPgm b state 
                           else runPgm (ptr+3) state
            7 -> if c <  b then runPgm (ptr+4) (state & (element a) .~ 1) 
                           else runPgm (ptr+4) (state & (element a) .~ 0)
            8 -> if c == b then runPgm (ptr+4) (state & (element a) .~ 1) 
                           else runPgm (ptr+4) (state & (element a) .~ 0)
            99 -> return state
            de -> runPgm (ptr+4) (state & (element a) .~ (getOp de c b))
            
main :: IO ()
main = do 
    file <- readFile "d5"
    nums <- return . read $ "[" ++ file ++ "]" :: IO [Int]
    exec <- runPgm 0 nums
    print $ exec
    
