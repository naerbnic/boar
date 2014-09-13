{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleInstances #-}
module GraphTest (htf_thisModulesTests) where

import Test.Framework
import Test.QuickCheck
import           Graph (Graph, Node, Edge)
import qualified Graph as G
import Control.Monad (replicateM)
import qualified Data.Map as M
import Debug.Trace

uniqueVectorOf :: (Show b, Ord a) => (b -> a) -> Int -> Gen b -> Gen [b]
uniqueVectorOf f n gb = go M.empty n
  where
    go m i = if i == 0
      then return (M.elems m)
      else do
        newB <- gb
        let k = f newB
        if k `M.member` m
          then go m i
          else go (M.insert k newB m) (i - 1)

instance (Show n, Show e, Arbitrary n, Arbitrary e) => Arbitrary (Graph Int n e) where
  arbitrary = sized $ \size -> do
    let keys = [1..size]
    ns <- vectorOf size arbitrary
      
    eSize <- choose (0 :: Int, size)
    es <- uniqueVectorOf G.edgeKey eSize $ do
      from <- elements keys
      to <- elements keys
      e <- arbitrary
      return $ G.Edge (G.EdgeKey from to) e
    return $ G.create (zipWith G.Node keys ns) es


g1 :: Graph Int () ()
g1 = G.singleton (G.Node 0 ())

test_singletonNodes :: IO ()
test_singletonNodes = assertEqual (G.nodes g1) [G.Node 0 ()]

test_singletonEdges :: IO ()
test_singletonEdges = assertEqual (G.edges g1) []

prop_reverse :: Graph Int () () -> Bool
prop_reverse g = G.reverse (G.reverse g) == g

eqCombine :: (Eq a) => G.CombineFunc a
eqCombine a b = if a == b then a else error "Values are not equal"

generateLoop :: Int -> Graph Int Int ()
generateLoop n = G.unfold
    id
    traverse
    eqCombine
    eqCombine
    0
  where
    traverse i =
      if i + 1 == n
        then [(0, ())]
        else [(i + 1, ())]

test_generatesCorrectly :: IO ()
test_generatesCorrectly =
  assertEqual (length (G.nodes (generateLoop 10))) 10