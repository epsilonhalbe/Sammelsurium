{-# OPTIONS_GHC
  -fno-warn-unused-binds
  -fno-warn-unused-matches
#-}

module Main where

import Control.Applicative ((<$>))
import Data.List (minimumBy)

main :: IO ()
main = do n <- read <$> getLine :: IO Int
          input <- map read . take n .words <$> getContents ::IO [Int]
          print $ miximum input
          return ()

miximum :: [Int] -> Int
miximum x | null x    = 0
          | otherwise = minimumBy cmpAbs x

cmpAbs x y | abs x <  abs y = LT
           | abs x >  abs y = GT
           | abs x == abs y && x == y = EQ
           | abs x == abs y && x <  y = GT
           | abs x == abs y && x >  y = LT

-- helper stuff
(∈) :: (Eq a) => a -> [a] -> Bool
(∈) = elem
