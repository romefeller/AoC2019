module Day11 where

import Control.Lens
import qualified Day9 as D9

data Dir = U | D | L | R deriving Show

type ProgramResult = (Integer, Integer, Int, D9.State, Integer)

type Coord = (Int,Int)

up,down,le,ri :: (Double,Double)
up = (0,1)
down = (0,-1)
le = (-1,0)
ri = (1,0)

(>+<) :: Coord -> Coord -> Coord
(>+<) (a,b) (c,d) = (a+c,b+d)

changeDir :: Int -> (Double,Double) -> (Double,Double)
changeDir t (x,y) = (x*cos theta - y*sin theta,x*sin theta + y*cos theta) 
    where
        theta = 0.5*pi* fromIntegral (-2*t+1)        

paint :: Integer
      -> Integer
      -> Int
      -> D9.State
      -> IO (Integer,ProgramResult)
paint r rb ptr state = do
     (rb',move,p,s,_) <- D9.runPgm rb D9.Feedback 0 [r] ptr state
     res <- D9.runPgm rb' D9.Feedback 0 [move] p state
     return (move,res)

iterPaint _ _ _ _ wh bl _ _ 99 = return (wh,bl)
iterPaint r rb ptr state wh bl initPoint initDir _ = do
    (move,(rb',color,p,s,opc)) <- paint r rb ptr state
    let
        (x,y) = changeDir (fromIntegral move) initDir
        newP = initPoint >+< (round x, round y)
    if color == 0 then
       iterPaint color rb ptr state wh (newP:bl) newP (x,y) opc
    else
       iterPaint color rb ptr state (newP:wh) bl newP (x,y) opc
    
    
main :: IO ()
main = do 
    file <- readFile "d11"
    nums <- return . read $ "[" ++ file ++ "]" :: IO [Integer]
    exec <- iterPaint 0 0 0 nums [] [] (0,0) (0,1) (-1)
    print $ exec
