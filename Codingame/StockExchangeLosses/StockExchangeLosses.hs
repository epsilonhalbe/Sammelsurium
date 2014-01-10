module Main where

import Control.Applicative ((<$>))

main :: IO ()
main = do n <- read <$> getLine :: IO Int
          input <- map read . take n . words <$> getContents ::IO [Int]
          let maximums = scanl1 max input
              mx = maximum $ zipWith (-) maximums input :: Int
          print $ negate (max 0 mx)
          return ()
