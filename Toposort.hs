module Toposort where

import Data.Foldable
import Data.Graph

topoSortWith
  :: (Foldable t, Foldable t', Ord k)
  => (a -> k)
  -> (a -> t' k)
  -> t a
  -> [SCC a]
topoSortWith fak fatk as
  = stronglyConnComp [(a, fak a, toList $ fatk a) | a <- toList as]

topoSort
  :: (Foldable t, Foldable t', Ord a)
  => t (a, t' a)
  -> [SCC a]
topoSort xs = stronglyConnComp [(k, k, toList ys) | (k, ys) <- toList xs]

cycles
  :: (Foldable t, Foldable t', Ord a)
  => t (a, t' a)
  -> [[a]]
cycles xs = concatMap cyclicGroup (topoSort xs)
  where
    cyclicGroup (CyclicSCC ys) = [ys]
    cyclicGroup (AcyclicSCC _) = []
