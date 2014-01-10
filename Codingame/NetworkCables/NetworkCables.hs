module Main where

import Control.Arrow ((&&&))
import Control.Applicative ((<$>))
import Data.List (sortBy)
import Data.Ord (comparing)

main :: IO ()
main = do n <- read <$> getLine :: IO Int
          input <- map ((\[x,y]->(x,y)). map read . words) . take n . lines <$> getContents ::IO [(Int,Int)]
          let l      = length input
              (hd,lt)= maximum &&& minimum $ map fst input
              modus  = snd $ sortBy (comparing snd) input !! (l `div` 2)
              diffs  = map (\x -> abs $ snd x - modus) input
              a      = abs (hd-lt) + sum diffs
          print a
