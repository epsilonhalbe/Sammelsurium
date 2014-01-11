{-# OPTIONS_GHC
  -fno-warn-unused-binds
  -fno-warn-unused-matches
#-}

module Algorithms where

import Graph.Data
import qualified Data.Map as Map
import qualified Data.Set as Set

type Level a = Set.Set a
type Parents a = Map.Map a (Set.Set a)

join :: (Ord a) => Set.Set (Set.Set a) -> Set.Set a
join = Set.foldl' Set.union Set.empty

bfs :: (Ord a) => a -> DGraph a -> [Level a]
-- | bfs = breadth first search is an algorithm to generate a level set of steps
-- from one starting point to every other node in the graph, one can generate a
-- parents tree of consisting shortests paths, which is not yet implemented
-- the steps in the algorithm are the following:
-- Step 1: from the current level set calculate all connected neighbours
-- Step 2: don't take the neighbours that are already in the previous level sets
-- Step 3: recurse on the current neghbour set and old level set added to acc
-- -----------------------------------------------------------------------------
-- I guess one could optimise the procedure by manipulating the graph such that
-- the previous levels do not show up in the lookup procedure
bfs start = loop [] (Set.singleton start)
    where loop :: (Ord a) => [Level a] -> Set.Set a -> DGraph a -> [Level a]
          loop acc ss g = let nbhdSet = fromMaybeSet . (`Map.lookup` adj g)
                              l  = join $ Set.map nbhdSet ss
                              l' = l Set.\\ Set.unions acc
                          in if Set.null l'
                               then reverse acc
                               else loop (ss:acc) l' g

dfs :: a -> a -> DGraph a -> Map.Map a [a]
dfs src trgt g = undefined


fromMaybeSet :: Maybe (Set.Set a) -> Set.Set a
fromMaybeSet Nothing = Set.empty
fromMaybeSet (Just s) = s
