module Main where

import Control.Applicative ((<$>))
import Data.Char (toUpper)
import Data.List (transpose)
{-import Data.List.Split (chunksOf)-}
import Data.Maybe (catMaybes)
import qualified Data.Map as M

main :: IO ()
main = do w <- read <$> getLine :: IO Int
          h <- read <$> getLine :: IO Int
          wrd <- getLine :: IO String
          alphabet <- map (chunksOf w) . lines <$> getContents ::IO [[String]]
          let translate = M.fromList $ zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ?" (transpose alphabet)
              output = transpose . catMaybes $ map ((`M.lookup` translate) . subst) wrd
          putStr $ unlines $ map concat output

subst :: Char -> Char
subst x | x ∈ ['A'..'Z'] = x
        | x ∈ ['a'..'z'] = toUpper x
        | otherwise      = '?'


-- {- Data.List.Split is not supported on Codingame -}
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

-- helper stuff
(∈) :: (Eq a) => a -> [a] -> Bool
(∈) = elem
