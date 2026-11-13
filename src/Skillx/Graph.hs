-- | The directed link graph of a skill store.
--
-- This module is pure. A 'Graph' is a 'Monoid': build one 'Graph' for each
-- skill with 'singleton' and combine them with '<>'.
--
-- The constructor of 'Graph' is not exported, so the reverse index is always
-- the exact transpose of the forward index.
module Skillx.Graph
  ( -- * Graph
    Graph,
    singleton,

    -- * Queries
    Dir (..),
    nodes,
    successors,
    predecessors,
    neighbours,

    -- * Traversal and integrity
    reachable,
    dangling,
    cycles,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Skillx.Name (Name)

-- | A directed graph over 'Name'.
--
-- Invariants:
--
-- * Every added node is a key of 'fwd', even when it has no outgoing edges.
-- * 'rev' is the exact transpose of 'fwd'.
-- * An edge may point at a 'Name' that is not a key of 'fwd'. See 'dangling'.
data Graph = Graph
  { -- | Node to the nodes that it links to.
    fwd :: !(Map Name (Set Name)),
    -- | Node to the nodes that link to it.
    rev :: !(Map Name (Set Name))
  }
  deriving (Eq, Show)

instance Semigroup Graph where
  Graph f1 r1 <> Graph f2 r2 = Graph (merge f1 f2) (merge r1 r2)
    where
      merge = Map.unionWith Set.union

instance Monoid Graph where
  mempty = Graph Map.empty Map.empty

-- | The graph of one skill and its outgoing links.
--
-- Targets of outgoing links do not become nodes until their own 'singleton'
-- is combined into the graph.
singleton :: Name -> Set Name -> Graph
singleton n targets =
  Graph
    { fwd = Map.singleton n targets,
      rev = Map.fromListWith Set.union [(t, Set.singleton n) | t <- Set.toList targets]
    }

-- | All nodes in the graph. Dangling targets are not included.
nodes :: Graph -> Set Name
nodes g = Map.keysSet (fwd g)

-- | The nodes that this node links to.
successors :: Name -> Graph -> Set Name
successors n g = Map.findWithDefault Set.empty n (fwd g)

-- | The nodes that link to this node.
predecessors :: Name -> Graph -> Set Name
predecessors n g = Map.findWithDefault Set.empty n (rev g)

-- | The direction of an edge relative to a node.
data Dir
  = -- | This node links to the other node.
    Out
  | -- | The other node links to this node.
    In
  deriving (Eq, Ord, Show)

-- | All incident edges of a node, in both directions.
neighbours :: Name -> Graph -> Set (Dir, Name)
neighbours n g =
  Set.map (Out,) (successors n g)
    <> Set.map (In,) (predecessors n g)

-- | All nodes reachable from the given root set along outgoing edges.
--
-- The result includes the roots themselves and terminates even when the graph
-- contains cycles. Dangling targets reachable from the roots are included.
reachable :: Set Name -> Graph -> Set Name
reachable roots g = go roots roots
  where
    go visited frontier
      | Set.null frontier = visited
      | otherwise = go (visited <> new) new
      where
        new = foldMap (`successors` g) frontier `Set.difference` visited

-- | Targets of edges that are not nodes of the graph.
dangling :: Graph -> Set Name
dangling g = Map.keysSet (rev g) `Set.difference` Map.keysSet (fwd g)

-- | All nodes that participate in at least one directed cycle.
--
-- A node @n@ is in a cycle if and only if @n@ is reachable in one or more
-- non-empty forward steps from itself.
cycles :: Graph -> Set Name
cycles g =
  Set.filter inCycle (nodes g)
  where
    inCycle n =
      let step1 = successors n g
       in n `Set.member` reachable step1 g
