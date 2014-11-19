module Boar.Generate.ProdStateSet where

import Boar.Generate.ProdState
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Boar.Data.MultiMap as MM
import Boar.Base.Rule (Rule)

-- Helpers

catMaybeSet :: Ord a => Set (Maybe a) -> Set a
catMaybeSet = Set.fromList . catMaybes . Set.toList

setMapMaybe :: Ord b => (a -> Maybe b) -> Set a -> Set b
setMapMaybe f s = catMaybeSet $ Set.map f s

recombine :: (Ord a, Ord b) => Set (a, b) -> Map a (Set b)
recombine = MM.toMap . MM.from

-- ProdStateSet

newtype ProdStateSet a = ProdStateSet (Set (ProdState a))

nexts :: Ord a => ProdStateSet a -> [(a, ProdStateSet a)]
nexts (ProdStateSet st) = let
  rulePairs = setMapMaybe step st
  recombinedSets = recombine rulePairs
  in Map.toList $ Map.map ProdStateSet recombinedSets
  
completeOfSet :: Ord a => Set (ProdState a) -> Set (Rule a)
completeOfSet =
  setMapMaybe $ \ps@(ProdState r _) ->
    if complete ps then Just r else Nothing