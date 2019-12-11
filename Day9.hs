module Day9 where

--import Text.Regex
import qualified Data.Text as T
import Data.Sequence
import Prelude hiding (take,replicate,drop)

import Control.Lens hiding (index)

leadZero opc = getDigits opc >< replicate 4 0

type State = Seq Integer

infixr 8 >***<  
(>***<) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
(>***<) f g = \(a,b) -> (f a, g b)

getDigits :: Integer -> Seq Integer
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

paramMode :: Integer -> Integer -> Integer -> Integer -> Seq Integer -> Integer
paramMode rb opcode digit inp state 
    | digit == 4 && elem res [0,2] = (if res == 2 then rb else 0) + inp
    | res == 2 && elem (mod opcode 100) [3]  = rb + inp 
    | elem res [0,2] = index state  (fromIntegral ((if res == 2 then rb else 0) + inp))
    | res == 1 = inp 
    where 
        res = index (leadZero opcode) (fromIntegral digit)
        
runPgm :: Integer -> PgmMode -> Integer -> Seq Integer -> Int -> State -> IO (Integer,Integer,Int,State,Integer)  
runPgm rb pm o r ptr state = 
    let 
        opcode = index state  ptr
        inp1 = index state   (ptr+1)   
        inp2 = index state   (ptr+2)
        out =  index state   (ptr+3)
        c = paramMode rb opcode 2 inp1 state 
        b = paramMode rb opcode 3 inp2 state
        a = paramMode rb opcode 4 out state
    in
        case (mod opcode 100) of
            3 -> runPgm rb pm o (drop 1 r) (ptr+2) (state & (element $ (fromIntegral c)) .~ (index r 0))
            4 -> case pm of  
                    Normal -> print c >> 
                              runPgm rb pm c r (ptr+2) state
                    Feedback -> return (rb,c,ptr+2,state,opcode)
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
            99 -> return (rb,o,ptr,take 1000 state,opcode)
            de -> runPgm rb pm o r (ptr+4) (state & (element $ fromIntegral a) .~ (getOp de c b))
            
main :: IO ()
main = do 
    file <- readFile "d9"
    nums <- return . read $ "[" ++ file ++ "]" :: IO [Integer]
    exec <- runPgm 0 Normal 0 (singleton 2) 0 (fromList nums >< replicate 1000 0)
    print $ exec
    
