module Main where

import Control.Applicative ((<$>))
import Data.List (sort)
-- import Debug.Trace

data Trie a = Trie a [Trie a]
            | Empty

instance Show a => Show (Trie a) where
    show Empty       = "\n"
    show (Trie t tt) = show t ++ ":"  ++ unlines (map show tt)

showMe :: Trie Char -> String
showMe Empty = "\n"
showMe (Trie t tt) = t: concatMap showMe tt

main :: IO ()
main = do _n <- read <$> getLine :: IO Int
          numbers <- sort . take _n . lines <$> getContents ::IO [String]
          print $ sum (map enumerate $ mkTrie numbers)
          return ()

mkTrie :: Eq a => [[a]] -> [Trie a]
mkTrie = foldr condInsert []

insert :: Eq a => [a] -> Trie a -> Trie a
insert [] t = t
insert (x:xs) Empty = Trie x [insert xs Empty]
insert (x:xs) tt@(Trie t ts) = if x==t then Trie t (condInsert xs ts)
                                          else tt

condInsert :: Eq a => [a] -> [Trie a] -> [Trie a]
condInsert []       tt = tt
condInsert xx@(x:_) tt = if x `elem` nodes tt then map (insert xx) tt
                                              else insert xx Empty : tt

nodes :: [Trie a] -> [a]
nodes [] = []
nodes (Empty:tt) = nodes tt
nodes (Trie x _:tt) = x:nodes tt

enumerate :: Trie a -> Int
enumerate Empty = 0
enumerate (Trie _ ts) = 1 + sum (map enumerate ts)
