module Data where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Monoid

data DGraph v = DGraph {vertices :: Set.Set v, adj :: Map.Map v (Set.Set v)}

empty :: DGraph v
empty = DGraph Set.empty Map.empty

singleton :: Ord v => (v,v) -> DGraph v
singleton (v,w) = DGraph (Set.fromList [v,w]) (Map.fromList [(v, Set.singleton w)])

data Edge = Tree | Backwards | Forwards | Cross

instance (Ord v) => Monoid (DGraph v) where
    mempty = empty
    mappend (DGraph vs es) (DGraph ws fs) = DGraph (vs `Set.union` ws) (Map.unionWith Set.union es fs)
