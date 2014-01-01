module Main where

import Control.Applicative ((<$>))
import Data.List (sort)
--import Control.Arrow

main :: IO ()
main = do input <- sort . map read . lines <$> getContents :: IO [Int]
          print . minimum $ zipWith subtract input (tail input)
