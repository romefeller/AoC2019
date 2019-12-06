module Day5 where

import Text.Regex
import qualified Data.Text as T
import Data.List

import Control.Lens 

leadZero opc = if opc < 10000 then '0' : show opc else show opc

type State = [Int]

getOp :: Int -> (Int -> Int -> Int)
getOp 1 = (+) 
getOp 2 = (*) 
getOp x = error $ show x

runPgm :: State -> State -> IO State
runPgm (3:i:next) state = runPgm next (state & (element i) .~ 1)
runPgm (4:x:next) state = print x >> runPgm next state
runPgm (99:_) state = return state 
runPgm (opcode:inp1:inp2:out:next) state = 
    runPgm next (state & (element $ state !! out) .~ (getOp de c b))
        where
           de = mod opcode 10
           c = if leadZero opcode !! 2 == '1' then inp1 else (state !! inp1)
           b = if leadZero opcode !! 1 == '1' then inp2 else (state !! inp2)
           --a = if leadZero opcode !! 0 == '0' then out else (state !! out)
main :: IO ()
main = do 
    file <- readFile "d5"
    nums <- return . read $ "[" ++ file ++ "]" :: IO [Int]
    exec <- runPgm nums nums
    print $ exec
    
