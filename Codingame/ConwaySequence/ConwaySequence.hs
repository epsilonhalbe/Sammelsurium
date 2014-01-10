module Main where

import Control.Applicative ((<$>))
import Data.List (group)

main :: IO ()
main = do start <- read <$> getLine :: IO Int
          line  <- read <$> getContents ::IO Int
          let conway = [start] : map (concatMap (\x -> [length x, head x]) . group ) conway :: [[Int]]
--          print $ length . unwords . map show $ conway !! (line-1)
          putStrLn $ unwords . map show $ conway !! (line-1)
