{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Boar.Data.MultiMap
  ( MultiMap()
  , toMap
  , mapValues
  , mapValuesWith
  , singleton
  , singletonKey
  , empty
  , union
  , unions
  , member
  , from
  , fromList
  , fromKeySet
  , toList
  , (!)
  , invert
  , groupValues

  , transitiveClosure
  ) where

import           Control.Arrow (second)
import           Data.Foldable (Foldable)
import qualified Data.Foldable as F
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Monoid   (Monoid (..))
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Data.Tuple    (swap)
import           Boar.Util.Fixpoint

-- | A structure that maps each value in the domain to zero or more values in
-- the range.
newtype MultiMap a b = MultiMap { toMap :: Map a (Set b) }
  deriving (Eq, Ord)

instance (Show k, Show v, Ord k, Ord v) => Show (MultiMap k v) where
  show v = "fromList " ++ (show $ toList v)

instance (Ord k, Ord v) => Monoid (MultiMap k v) where
  mempty = MultiMap M.empty
  mappend (MultiMap m1) (MultiMap m2) =
    MultiMap $ M.unionWith S.union m1 m2

-- Utilities

mapSet :: (Ord b) => (a -> Set b) -> Set a -> Set b
mapSet f as = S.unions $ map f (S.toList as)

foldableToSet :: (Ord a, Foldable t) => t a -> Set a
foldableToSet as = S.fromList $ F.toList as

keyValueToAssoc :: (Ord b) => a -> Set b -> [(a, b)]
keyValueToAssoc k = map (\v -> (k, v)) . S.toList

-- Functions

mapValuesWith :: (Ord a, Ord b, Ord c)
              => (a -> b -> Set c)
              -> MultiMap a b
              -> MultiMap a c
mapValuesWith f (MultiMap ins) =
  MultiMap $ M.filter (not . S.null) $ M.mapWithKey (mapSet . f) ins

mapValues :: (Ord a, Ord b, Ord c) => (b -> Set c) -> MultiMap a b -> MultiMap a c
mapValues f = mapValuesWith (const f)

singleton :: (Ord a, Ord b) => a -> b -> MultiMap a b
singleton a b = MultiMap $ M.singleton a (S.singleton b)

singletonKey :: (Ord a, Ord b, Foldable t) => a -> t b -> MultiMap a b
singletonKey a bs =
  let bset = foldableToSet bs
  in if S.null bset
     then empty
     else MultiMap $ M.singleton a bset

empty :: (Ord a, Ord b) => MultiMap a b
empty = mempty

union :: (Ord a, Ord b) => MultiMap a b -> MultiMap a b -> MultiMap a b
union = mappend

unions :: (Ord a, Ord b, Foldable t) => t (MultiMap a b) -> MultiMap a b
unions = F.foldr union empty

member :: (Ord a, Ord b) => (a, b) -> MultiMap a b -> Bool
member (a, b) (MultiMap m) = case M.lookup a m of
  Nothing -> False
  Just bs -> b `S.member` bs

(!) :: (Ord a, Ord b) => MultiMap a b -> a -> Set b
(MultiMap m) ! a = case M.lookup a m of
  Nothing -> S.empty
  Just b -> b

fromList :: (Ord a, Ord b) => [(a, b)] -> MultiMap a b
fromList l = MultiMap $ M.fromListWith S.union (map (second S.singleton) l)

toList :: (Ord a, Ord b) => MultiMap a b -> [(a, b)]
toList (MultiMap m) = M.foldWithKey (\a bs elems -> keyValueToAssoc a bs ++ elems) [] m

from :: (Ord a, Ord b, Foldable t) => t (a, b) -> MultiMap a b
from fld = fromList $ F.toList fld

fromKeySet :: (Ord a, Ord b) => (a -> Set b) -> Set a -> MultiMap a b
fromKeySet f = MultiMap . M.fromList . filter (\(_, y) -> not $ S.null y) . map (\x -> (x, f x)) . S.toList

invert :: (Ord a, Ord b) => MultiMap a b -> MultiMap b a
invert = fromList . map swap . toList

groupValues :: (Ord a, Ord b) => (b -> a) -> Set b -> MultiMap a b
groupValues f s = fromList $
  map (\x -> (f x, x)) $
  S.toList s

-- Fixpoints

transitiveClosure :: (Ord a) => MultiMap a a -> MultiMap a a
transitiveClosure = fixpointEq iter
  where
    iter m = mapValues (m !) m
