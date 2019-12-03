module Day3 where

import Text.Regex
import qualified Data.Text as T

main :: IO ()
main = do 
    --let r = mkRegex "\\[([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})\\] 
    --(falls asleep|wakes up|Guard #([0-9]+) begins shift)"
    --acts <- return $ . matchRegex r . T.unpack)
    file <- readFile "d3"
    print $ file
