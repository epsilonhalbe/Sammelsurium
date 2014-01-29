{-# OPTIONS_GHC
  -fno-warn-unused-binds
  -fno-warn-unused-matches
#-}

module Algorithms where

import Data
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

dfs :: (Ord a) => a -> DGraph a -> Map.Map a [(a,Edge)]
-- | dfs = depth first search is an algorithm to walk to every node in a given
-- tree starting from one node - during this procedure every edge is visited and
-- can be classified to be one of the following: Tree-Edge
--                                               Forward-Edge
--                                               Back-Edge
--                                               Cross-Edge
--
-- The algorithm is calculated in the following three steps:
-- Step 1: take a node follow edge and make a zipper structure of parent for
--         backtracking, make sure that you don't visit any previous nodes/edges
-- Step 2: repeat [Step 1] until you hit bottom
-- Step 3: go back to the first place of choice and repeat Step 1/Step 2
dfs src' g' = loop Map.empty Set.empty (vertices g') src' g'
    where loop :: (Ord a) => Map.Map a [(a,Edge)] -> Set.Set a -> Set.Set a -> a -> DGraph a -> Map.Map a [(a,Edge)]
          loop acc avail zpp src g
               = if Set.null avail
                   then acc
                   else let nbs' = src `Map.lookup` adj g
                        in case nbs' of Nothing  -> error "current key not in graph"
                                        Just nbs -> Map.empty


fromMaybeSet :: Maybe (Set.Set a) -> Set.Set a
fromMaybeSet Nothing = Set.empty
fromMaybeSet (Just s) = s









