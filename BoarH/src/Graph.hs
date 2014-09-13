{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Graph
  ( CombineFunc
  , Graph()
  , Node(..)
  , EdgeKey(..)
  , Edge(..)
  , empty
  , create
  , nodes
  , edges
  , nodeKeys
  , edgeKeys
  , singleton

  , addNode
  , addNodes
  , addEdge
  , addEdges

  , getNodeData

  , unfold

  , reverse

  ) where

import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import           Fixpoint
import           Prelude  hiding (reverse)

-- Helpers
----------

isListUnique :: Ord a => [a] -> Bool
isListUnique l = S.size (S.fromList l) == length l

check :: Bool -> String -> a -> a
check prop msg a = if prop
                   then a
                   else error msg

-- Graph data structures
------------------------

-- | A function which combines two things of the same type into one. Each
-- such function @f@ must follow the following laws:
--
-- 1. /Commutative/: @a `f` b = b `f` a@
-- 2. /Associative/: @(a `f` b) `f` c = a `f` (b `f` c)@
-- 3. /Reflexive-Identity/: @ (a `f` a) = a @
type CombineFunc a = (a -> a -> a)

data NodeImpl k n e = NodeImpl
  { nodeImplData :: n
  , edgeMap      :: Map k e
  } deriving (Ord, Eq, Show)

{- |

A directed graph. Each graph contains

* A set of nodes indexed by a value of type @k@
* A value of type @n@ for each node.
* A collection of edges going from one node to another
* A value of type @e@ for each edge.

@k@ must implement @Ord@. No two nodes have the same key, nor do any two
edges have the same pair of from and to nodes.

-}
data Graph k n e = Graph
  { getNodeMap :: Map k (NodeImpl k n e)
    -- ^ Testing.
  } deriving (Ord, Eq, Show)

-- | A single node in a graph. It is indexed by a key, and contains a datum.
data Node k n = Node
  { nodeKey  :: k
    -- ^ The key for the given node.
  , nodeData :: n
    -- ^ The datum for the given node.
  } deriving (Ord, Eq, Show)

-- | The key for an edge. It contains the key from the node this edge comes
-- from, and the key for the node this edge goes to.
data EdgeKey k = EdgeKey
  { from :: k
    -- ^ The key of the node this edge comes from.
  , to   :: k
    -- ^ The key of the node this edge goes to.
  } deriving (Ord, Eq, Show)

-- | A single edge in a graph. It is indexed by a key, and contains a datum.
data Edge k e = Edge
  { edgeKey  :: EdgeKey k
    -- ^ The key for this edge.
  , edgeData :: e
    -- ^ The data for this edge.
  } deriving (Ord, Eq, Show)

-- | Returns an empty graph.
empty :: Graph k n e
empty = Graph M.empty

{- |
Creates a new graph from the input nodes and edges.

A call @create nodes edges@ will throw an error if:

* There exist two nodes in @nodes@ that have the same key
* There exist two edges in @edges@ that have the same key
* There is a key in @edges@ that refers to a node key that is not in @nodes@
-}
create :: Ord k => [Node k n] -> [Edge k e] -> Graph k n e
create ns es =
    check nodesUnique "Nodes do not have unique keys" $
    check edgesUnique "Edges do not have unique keys" $
    check edgesAreValid "Not all nodes referenced by edges exist" $
    addEdges const es $ addNodes const ns empty
  where
    nodesUnique = isListUnique $ map nodeKey ns
    edgesUnique = isListUnique $ map edgeKey es
    referencedKeys = S.fromList $
      map (from . edgeKey) es ++ map (to . edgeKey) es

    edgesAreValid = referencedKeys `S.isSubsetOf` S.fromList (map nodeKey ns)

-- | Returns a list of nodes contained in the graph.
nodes :: Ord k => Graph k n e -> [Node k n]
nodes (Graph m) = map (\(k, ni) -> Node k (nodeImplData ni)) $ M.toList m

-- | Returns a list of node keys contained in the graph.
nodeKeys :: Ord k => Graph k n e -> [k]
nodeKeys = map nodeKey . nodes

-- | Returns a list of edges contained in the graph.
edges :: Ord k => Graph k n e -> [Edge k e]
edges (Graph m) = concatMap createEdges (M.toList m)
  where
    createEdges (fromN, ni) = map (\(toN, e) -> (Edge (EdgeKey fromN toN) e)) $ M.toList $ edgeMap ni

-- | Returns a list of edge keys contained in the graph.
edgeKeys :: Ord k => Graph k n e -> [EdgeKey k]
edgeKeys = map edgeKey . edges

-- | Returns true if the graph contains a node with the given key
containsNode :: Ord k => Graph k n e -> k -> Bool
containsNode (Graph m) k = k `M.member` m

-- | Returns a graph that contains a single node
singleton :: Ord k => Node k n -> Graph k n e
singleton n = Graph $ M.singleton (nodeKey n) (NodeImpl (nodeData n) M.empty)

-- | Adds a single edge to the graph. The nodes referenced by the edge's key
-- must exist in the graph.
--
-- If an edge already exists with the given pair, the edge combiner is used to
-- merge the pair of edge values. The order of this application is undefined.
addEdge :: Ord k => CombineFunc e -> Edge k e -> Graph k n e -> Graph k n e
addEdge combineE e g@(Graph m) =
  check (containsNode g f && containsNode g t)
      "Graph does not contain referenced nodes" $
  Graph $ M.adjust modifyNode f m
  where
    f = from (edgeKey e)
    t = to (edgeKey e)

    modifyNode (NodeImpl n edgeM) =
        NodeImpl n (M.insertWith combineE t (edgeData e) edgeM)

-- | Adds all of the edges in the given edge list to the graph.
-- The nodes referenced by the edges' keys must exist in the graph.
--
-- If an edge already exists with the given pair, the edge combiner is used to
-- merge the pair of edge values.
addEdges :: Ord k => CombineFunc e -> [Edge k e] -> Graph k n e -> Graph k n e
addEdges combineE edgeList g = foldr (addEdge combineE) g edgeList

-- | Adds a single node to the graph.
--
-- If an node already exists with the given pair, the node combiner is used to
-- merge the pair of node values. The order of this application is undefined.
addNode :: Ord k => CombineFunc n -> Node k n -> Graph k n e -> Graph k n e
addNode combineN n (Graph m) =
    Graph $ M.alter alt (nodeKey n) m
  where
    alt Nothing = Just $ NodeImpl (nodeData n) M.empty
    alt (Just (NodeImpl n' eMap)) = Just $ NodeImpl (combineN (nodeData n) n') eMap

-- | Adds all nodes in the list to the graph.
--
-- If an node already exists with the given pair, the node combiner is used to
-- merge the pair of node values. The order of this application is undefined.
addNodes :: Ord k => CombineFunc n -> [Node k n] -> Graph k n e -> Graph k n e
addNodes combineN nodeList g = foldr (addNode combineN) g nodeList

-- | If the node with the given key exists, returns the @Just@ of that value.
-- Otherwise, it returns nothing.
getNodeData :: Ord k => Graph k n e -> k -> Maybe n
getNodeData g k = do
  nimpl <- M.lookup k (getNodeMap g)
  return $ nodeImplData nimpl

extendGraph ::
    Ord k =>
    (n -> k) -> (n -> [(n, e)]) -> CombineFunc n -> CombineFunc e -> Graph k n e -> Graph k n e
extendGraph extract traverse combineN combineE g = g''
  where
    traverseLists = map (\(Node k n) -> (k, traverse n)) $ nodes g

    newNodes =
        map (\n -> Node (extract n) n) $
        concatMap (\ (_, v) -> map fst v) traverseLists

    newEdges = concatMap (\ (k, v) -> map (\ (n, e) -> Edge (EdgeKey k (extract n)) e) v)
                   traverseLists

    g' = addNodes combineN newNodes g

    g'' = addEdges combineE newEdges g'


-- | Creates a graph given an extractor, a traverser, and a combiner, create
-- an entire graph.
-- An extractor: Given a node's data, extract a key value from it.
-- A traverser: Given a node's data, return all edges out from it, and
-- their associated datums
-- An edge combiner: Given the edge data for an edge with the same from and
-- two keys, return a new edge
-- The seed: The starting node which will be used to derive the graph
-- A node combiner: Given two node datas with the same key, combine them to
-- form a new node data. Must be commutitive
unfold :: (Ord k, Eq n, Eq e)
  => (n -> k)        -- ^ The extractor
  -> (n -> [(n, e)]) -- ^ The traverser
  -> CombineFunc n   -- ^ The node combiner
  -> CombineFunc e   -- ^ The edge combiner
  -> n               -- ^ The seed
  -> Graph k n e     -- ^ The unfolded graph
unfold extract traverse combineN combineE seed =
    fixpointEq (extendGraph extract traverse combineN combineE)
               (singleton (dataToNode seed))
  where
    dataToNode n = Node (extract n) n

-- | Returns a new graph with all of the edge of this graph reversed.
reverse :: Ord k => Graph k n e -> Graph k n e
reverse g = create (nodes g) (map reverseEdge $ edges g)
  where reverseEdge (Edge (EdgeKey f t) e) = Edge (EdgeKey t f) e

