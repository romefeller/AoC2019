module Day9 where

import Text.Regex
import qualified Data.Text as T
import Data.List

import Control.Lens 

leadZero opc = getDigits opc ++ [0,0,0,0]

type State = [Integer]

infixr 8 >***<  
(>***<) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
(>***<) f g = \(a,b) -> (f a, g b)

getDigits :: Integer -> [Integer]
getDigits n = 
    unfoldr (\x -> if x == 0 then 
            Nothing 
        else 
            Just . (mod x >***< div x) $ (10,10)) n

getOp :: Integer -> (Integer -> Integer -> Integer)
getOp 1 = (+) 
getOp 2 = (*) 
getOp x = error $ show x

data PgmMode = Normal | Feedback

paramMode :: Integer -> Integer -> Integer -> Integer -> [Integer] -> Integer
paramMode rb opcode digit inp state 
    | digit == 4 && res == 0 = inp
    | res == 2 && (mod opcode 100) == 3  = rb + inp 
    | res == 2 = state !! fromIntegral (rb + inp)
    | res == 1 = inp 
    | res == 0 = state !! fromIntegral inp
    where 
        res = leadZero opcode !! (fromIntegral digit)
        
runPgm :: Integer -> PgmMode -> Integer -> [Integer] -> Int -> State -> IO (Integer,Int,State,Integer)  
runPgm rb pm o r ptr state = 
    let 
        opcode = state !! ptr
        inp1 = state   !! (ptr+1)   
        inp2 = state   !! (ptr+2)
        out =  state   !! (ptr+3)
        c = paramMode rb opcode 2 inp1 state 
        b = paramMode rb opcode 3 inp2 state
        a = paramMode rb opcode 4 out state
    in
        case (mod opcode 100) of
            3 -> runPgm rb pm o (tail r) (ptr+2) (state & (element $ (fromIntegral c)) .~ (head r))
            4 -> case pm of  
                    Normal -> print c >> 
                              runPgm rb pm c r (ptr+2) state
                    Feedback -> return (c,ptr+2,state,opcode)
            5 -> if c /= 0 then runPgm rb pm o r (fromIntegral b) state 
                           else runPgm rb pm o r (ptr+3) state 
            6 -> if c == 0 then runPgm rb pm o r (fromIntegral b) state 
                           else runPgm rb pm o r (ptr+3) state
            7 -> if c <  b then 
                    runPgm rb pm o r (ptr+4) (state & (element $ fromIntegral a) .~ 1) 
                else 
                    runPgm rb pm o r (ptr+4) (state & (element $ fromIntegral a) .~ 0)
            8 -> if c == b then 
                    runPgm rb pm o r (ptr+4) (state & (element $ fromIntegral a) .~ 1) 
                else 
                    runPgm rb pm o r (ptr+4) (state & (element $ fromIntegral a) .~ 0)
            9 -> runPgm (rb + c) pm o r (ptr+2) state
            99 -> return (o,ptr,take 1000 state,opcode)
            de -> runPgm rb pm o r (ptr+4) (state & (element $ fromIntegral a) .~ (getOp de c b))
            
main :: IO ()
main = do 
    file <- readFile "d9"
    nums <- return . read $ "[" ++ file ++ "]" :: IO [Integer]
    exec <- runPgm 0 Normal 0 [1] 0 (nums ++ repeat 0)
    print $ exec
    
