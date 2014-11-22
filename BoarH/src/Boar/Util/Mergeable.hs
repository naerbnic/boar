{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Boar.Util.Mergeable
  ( Mergeable(..)
  , MergeSet

  , fromList
  , toList
  , empty
  , null
  , size
  , singleton
  , filter
  , union
  , unions

  , extractKeySet
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Prelude  hiding (filter, null)

class Ord (MergeKey m) => Mergeable m where
  type MergeKey m :: *
  extractKey :: m -> MergeKey m
  merge :: m -> m -> m

newtype MergeSet m = MergeSet { extractMap :: Map (MergeKey m) m }

instance Mergeable m => Mergeable (MergeSet m) where
  type MergeKey (MergeSet m) = Set (MergeKey m)
  extractKey = extractKeySet
  merge = union

-- General operations

fromList :: Mergeable m => [m] -> MergeSet m
fromList mList =
  MergeSet $ Map.fromListWith merge $ map (\x -> (extractKey x, x)) mList

toList :: Mergeable m => MergeSet m -> [m]
toList (MergeSet mMap) = Map.elems mMap

empty :: MergeSet m
empty = MergeSet Map.empty

null :: MergeSet m -> Bool
null (MergeSet mMap) = Map.null mMap

size :: MergeSet m -> Int
size (MergeSet mMap) = Map.size mMap

singleton :: Mergeable m => m -> MergeSet m
singleton m = MergeSet $ Map.singleton (extractKey m) m

filter :: Mergeable m => (m -> Bool) -> MergeSet m -> MergeSet m
filter f (MergeSet mMap) = MergeSet $ Map.filter f mMap

union :: Mergeable m => MergeSet m -> MergeSet m -> MergeSet m
union (MergeSet mMap1) (MergeSet mMap2) =
  MergeSet $ Map.unionWith merge mMap1 mMap2

unions :: Mergeable m => [MergeSet m] -> MergeSet m
unions mList = MergeSet $ Map.unionsWith merge $ map extractMap mList

-- MergeSet Specific

extractKeySet :: Mergeable m => MergeSet m -> Set (MergeKey m)
extractKeySet (MergeSet mMap) = Set.fromAscList $ Map.keys mMap
