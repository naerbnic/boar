module Boar.Generate.ProdStateSet where

import Boar.Generate.ProdState
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Boar.Data.MultiMap as MM
import Boar.Base.Rule (Rule)
import Boar.Base.Grammar (Grammar)
import qualified Boar.Base.Grammar as G

-- Helpers

catMaybeSet :: Ord a => Set (Maybe a) -> Set a
catMaybeSet = Set.fromList . catMaybes . Set.toList

mapMaybeSet :: Ord b => (a -> Maybe b) -> Set a -> Set b
mapMaybeSet f s = catMaybeSet $ Set.map f s

concatMapSet :: (Ord a, Ord b) => (a -> Set b) -> Set a -> Set b
concatMapSet f = Set.unions . map f . Set.toList

recombine :: (Ord a, Ord b) => Set (a, b) -> Map a (Set b)
recombine = MM.toMap . MM.from

-- ProdStateSet

newtype ProdStateSet a = ProdStateSet (Set (ProdState a))
  deriving (Eq, Ord, Show)

nexts :: Ord a => ProdStateSet a -> [(a, ProdStateSet a)]
nexts (ProdStateSet st) = let
  rulePairs = mapMaybeSet step st
  recombinedSets = recombine rulePairs
  in Map.toList $ Map.map ProdStateSet recombinedSets
  
atPoints :: Ord a => ProdStateSet a -> Set a
atPoints = Set.fromList . map fst . nexts
  
completeOfSet :: Ord a => ProdStateSet a -> Set (Rule a)
completeOfSet (ProdStateSet s) =
  mapMaybeSet
    (\ps@(ProdState r _) -> if complete ps then Just r else Nothing)
    s