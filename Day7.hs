module Day7 where

import qualified Data.Text as T
import Data.List
import qualified Day5 as D
import Control.Arrow
import Control.Monad

type Feeds = (D.State,D.State,D.State,D.State,D.State)

type Ptrs = (Int,Int,Int,Int,Int)

type Feedback = ((D.State, D.State, D.State, D.State, D.State), [Int],
                 (Int, Int, Int, Int, Int))

phases :: D.State -> [Int] -> IO Int
phases pgm (a:b:c:d:e:[]) = do
    (b',_,_,_) <- D.runPgm D.Normal 0 [a,0] 0 pgm
    (c',_,_,_) <- D.runPgm D.Normal 0 [b,b'] 0 pgm
    (d',_,_,_) <- D.runPgm D.Normal 0 [c,c'] 0 pgm
    (e',_,_,_) <- D.runPgm D.Normal 0 [d,d'] 0 pgm
    (n,_,_,_)  <- D.runPgm D.Normal 0 [e,e'] 0 pgm
    return n

visited True (a:b:[]) = [b]
visited False as = as
    
feedback :: Bool -> (Int,Feedback) -> IO (Int,Feedback)  
feedback v (inp,((pA,pB,pC,pD,pE),(a:b:c:d:e:[]),(pt1,pt2,pt3,pt4,pt5))) = do
    (b',pt1',pA',_) <- D.runPgm D.Feedback 0 (visited v [a,inp]) pt1 pA
    (c',pt2',pB',_) <- D.runPgm D.Feedback 0 (visited v [b,b']) pt2 pB
    (d',pt3',pC',_) <- D.runPgm D.Feedback 0 (visited v [c,c']) pt3 pC
    (e',pt4',pD',_) <- D.runPgm D.Feedback 0 (visited v [d,d']) pt4 pD
    (out,pt5',pE',opc) <- D.runPgm D.Feedback 0 (visited v [e,e']) pt5 pE
    print $ inp
    if opc /= 99 then 
        feedback True (out,((pA',pB',pC',pD',pE'),(a:b:c:d:e:[]),(pt1',pt2',pt3',pt4',pt5')))
    else 
        return $ (inp,((pA',pB',pC',pD',pE'),(a:b:c:d:e:[]),(pt1',pt2',pt3',pt4',pt5')))

main :: IO ()
main = do 
    file <- readFile "d7"
    pgm <- return . read $ "[" ++ file ++ "]" :: IO [Int]
    res <- sequence $ phases pgm <$> (permutations [0 .. 4])
    print $ "Max th: " ++  show (maximum res)
    let params x = ((pgm,pgm,pgm,pgm,pgm),x,(0,0,0,0,0))
    all <- sequence $ curry (feedback False) 0 
                 <$> params  <$> (permutations [5 .. 9])
    print $ maximum $ fst <$> all
