{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Graph where

import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import           Fixpoint

isListUnique :: Ord a => [a] -> Bool
isListUnique l = S.size (S.fromList l) == length l

type CombineFunc a = (a -> a -> a)

data NodeImpl k n e = NodeImpl
  { nodeImplData :: n
  , edgeMap      :: Map k e
  } deriving (Ord, Eq, Show)

data Graph k n e = Graph
  { getNodeMap :: Map k (NodeImpl k n e)
  } deriving (Ord, Eq, Show)

data Node k n = Node
  { nodeKey      :: k
  , nodeData :: n
  } deriving (Ord, Eq, Show)

data EdgeKey k = EdgeKey
  { from :: k
  , to :: k
  } deriving (Ord, Eq, Show) 

data Edge k e = Edge
  { edgeKey :: EdgeKey k
  , edgeData :: e
  } deriving (Ord, Eq, Show)
  
empty :: Graph k n e
empty = Graph M.empty
  
create :: Ord k => [Node k n] -> [Edge k e] -> Graph k n e
create ns es = if nodesUnique && edgesUnique && edgesAreValid
    then addEdges const es $ addNodes const ns empty
    else error "Duplicate nodes/edges"
  where
    nodesUnique = isListUnique $ map nodeKey ns
    edgesUnique = isListUnique $ map edgeKey es
    referencedKeys = S.fromList $
      map (from . edgeKey) es ++ map (to . edgeKey) es
    
    edgesAreValid = referencedKeys `S.isSubsetOf` S.fromList (map nodeKey ns)
    

nodes :: Ord k => Graph k n e -> [Node k n]
nodes (Graph m) = map (\(k, ni) -> Node k (nodeImplData ni)) $ M.toList m

nodeKeys :: Ord k => Graph k n e -> [k]
nodeKeys = map nodeKey . nodes

edges :: Ord k => Graph k n e -> [Edge k e]
edges (Graph m) = concatMap createEdges (M.toList m)
  where
    createEdges (fromN, ni) = map (\(toN, e) -> (Edge (EdgeKey fromN toN) e)) $ M.toList $ edgeMap ni
    
edgeKeys :: Ord k => Graph k n e -> [EdgeKey k]
edgeKeys = map edgeKey . edges

singleton :: Ord k => k -> n -> Graph k n e
singleton k n = Graph $ M.singleton k (NodeImpl n M.empty)

addEdge :: Ord k => CombineFunc e -> Edge k e -> Graph k n e -> Graph k n e
addEdge combineE e (Graph m) = Graph $ M.adjust modifyNode (from $ edgeKey e) m
  where
    modifyNode (NodeImpl n eMap) = NodeImpl n (M.insertWith combineE (to $ edgeKey e) (edgeData e) eMap)

addEdges :: Ord k => CombineFunc e -> [Edge k e] -> Graph k n e -> Graph k n e
addEdges combineE edgeList g = foldr (addEdge combineE) g edgeList

addNode :: Ord k => CombineFunc n -> Node k n -> Graph k n e -> Graph k n e
addNode combineN n (Graph m) =
    Graph $ M.alter alt (nodeKey n) m
  where
    alt Nothing = Just $ NodeImpl (nodeData n) M.empty
    alt (Just (NodeImpl n' eMap)) = Just $ NodeImpl (combineN (nodeData n) n') eMap

addNodes :: Ord k => CombineFunc n -> [Node k n] -> Graph k n e -> Graph k n e
addNodes combineN nodeList g = foldr (addNode combineN) g nodeList

containsNodeKey :: Ord k => Graph k n e -> k -> Bool
containsNodeKey (Graph m) k = M.member k m

getNodeData :: Ord k => Graph k n e -> k -> Maybe n
getNodeData g k = do
  nimpl <- M.lookup k (getNodeMap g)
  return $ nodeImplData nimpl

mergeNode :: Ord k => CombineFunc n -> CombineFunc e ->
    NodeImpl k n e -> NodeImpl k n e -> NodeImpl k n e
mergeNode combineN combineE leftN rightN = NodeImpl
    { nodeImplData = apply combineN nodeImplData leftN rightN
    , edgeMap = apply (M.unionWith combineE) edgeMap leftN rightN
    }
  where apply binf f a b = binf (f a) (f b)

mergeGraph :: Ord k =>
    (n -> n -> n) -> (e -> e -> e) -> Graph k n e -> Graph k n e -> Graph k n e
mergeGraph nMerge eMerge leftG rightG =
  let apply binf f a b = binf (f a) (f b)
  in Graph $ apply (M.unionWith (mergeNode nMerge eMerge)) getNodeMap leftG rightG

mergeValues :: Ord b => (a -> b) -> (a -> a -> a) -> [a] -> Map b a
mergeValues extract combine values = M.fromListWith combine keyValues
  where
    keyValues = map (\v -> (extract v, v)) values

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
unfold :: (Ord k, Eq n, Eq e) =>
  -- | An extractor: Given a node's data, extract a key value from it
  (n -> k) ->
  -- | A traverser: Given a node's data, return all edges out from it, and
  -- their associated datums
  (n -> [(n, e)]) ->
  -- | A node combiner: Given two node datas with the same key, combine them to
  -- form a new node data. Must be commutative
  CombineFunc n ->
  -- | An edge combiner: Given the edge data for an edge with the same from and
  -- two keys, return a new edge
  CombineFunc e ->
  -- | The seed: The starting node which will be used to derive the graph
  n ->
  -- | A graph which has the following properties:
  Graph k n e
unfold extract traverse combineN combineE seed =
    fixpointEq (extendGraph extract traverse combineN combineE)
               (singleton (extract seed) seed)

reverse :: Ord k => Graph k n e -> Graph k n e
reverse g = create (nodes g) (map reverseEdge $ edges g)
  where reverseEdge (Edge (EdgeKey f t) e) = Edge (EdgeKey t f) e
  
