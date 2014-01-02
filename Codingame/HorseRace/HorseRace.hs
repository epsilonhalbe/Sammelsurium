module Main where

import Control.Applicative ((<$>))
import Data.List (sort)
--import Control.Arrow

main :: IO ()
main = do n <- read <$> getLine :: IO Int
          input <- sort . map read . take n . lines <$> getContents :: IO [Int]
          print . minimum $ zipWith subtract input (tail input)
