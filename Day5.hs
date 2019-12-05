module Day5 where

import Text.Regex
import qualified Data.Text as T
import Data.List

import Control.Lens 

leadZero opc = if opc < 10000 then '0' : show opc else show opc

type State = [Int]

data Op = Plus | Mult | Mode Int deriving Show

data IntCode = Modify Op Int Int Int | Readd Int | Output Int | End deriving Show

readPgm :: State -> [IntCode] 
readPgm [] = []
readPgm [x] = [End]
readPgm (opc:next) 
    | opc == 1 = let (inp1:inp2:out:_) = next in Modify Plus inp1 inp2 out : readPgm (drop 3 next)
    | opc == 2 = let (inp1:inp2:out:_) = next in Modify Mult inp1 inp2 out : readPgm (drop 3 next) 
    | opc == 3 = Readd  (head next) : readPgm (tail next)
    | opc == 4 = Output (head next) : readPgm (tail next)
    | opc == 99 = End : readPgm []
    | opc >= 100 = 
            let (inp1:inp2:out:_) = next 
            in Modify (Mode opc) inp1 inp2 out : readPgm (drop 3 next) 
    | otherwise = error $ show $ opc

getOp :: Op -> (Int -> Int -> Int)
getOp Plus = (+) 
getOp Mult = (*) 

intOp :: Int -> Op
intOp 1 = Plus
intOp 2 = Mult
intOp _ = error "out of bounds"

runPgm :: [IntCode] -> State -> IO State
runPgm ((Readd  i):next) state = runPgm next (state & (element i) .~ 1)
runPgm ((Output x):next) state = print x >> runPgm next state
runPgm (End:_) state = return state 
runPgm ((Modify (Mode opcode) inp1 inp2 out):next) state = 
    runPgm next (state & (element a) .~ (getOp de c b))
        where
           de = intOp $ mod opcode 10
           c = if leadZero opcode !! 2 == '1' then inp1 else (state !! inp1)
           b = if leadZero opcode !! 1 == '1' then inp2 else (state !! inp2)
           a = if leadZero opcode !! 0 == '0' then out else (state !! out)
runPgm ((Modify opcode inp1 inp2 out):next) state = 
    runPgm next (state & (element out) .~ (getOp opcode (state !! inp1) (state !! inp2)))
        
main :: IO ()
main = do 
    file <- readFile "d5"
    nums <- return . read $ "[" ++ file ++ "]" :: IO [Int]
    exec <- runPgm (readPgm nums) nums
    print $ exec
    
