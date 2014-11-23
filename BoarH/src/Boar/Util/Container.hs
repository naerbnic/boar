{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Boar.Util.Container where

import GHC.Exts (Constraint)
import Prelude hiding (concat)
import qualified Prelude as Pre
import qualified Data.Maybe as May
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
  
class Container co where
  type Elem co a :: Constraint
  
  empty :: Elem co a => co a
  null :: Elem co a => co a -> Bool
  singleton :: Elem co a => a -> co a
  concat :: Elem co a => [co a] -> co a
  size :: Elem co a => co a -> Int
  
  fromList :: Elem co a => [a] -> co a
  toList :: Elem co a => co a -> [a]
  
  mapMaybe :: (Elem co a, Elem co b) => (a -> Maybe b) -> co a -> co b
  
  map :: (Elem co a, Elem co b) => (a -> b) -> co a -> co b
  map f = mapMaybe (Just . f)
  
  filter :: (Elem co a) => (a -> Bool) -> co a -> co a
  filter f = mapMaybe (\x -> if f x then Just x else Nothing)
  
class Container co => Indexable co where
  type Key co a :: *
  type Value co a :: *
  
  index :: Elem co a => co a -> Key co a -> Maybe (Value co a)
  
-- Instances
  
instance Container [] where
  type Elem [] a = ()
  
  empty = []
  null = Pre.null
  singleton x = [x]
  concat = Pre.concat
  size = Pre.length
  
  fromList = id
  toList = id
  
  mapMaybe = May.mapMaybe
  map = Pre.map
  filter = Pre.filter
  
instance Indexable [] where
  type Key [] a = Int
  type Value [] a = a
  
  index [] _ = Nothing
  index (a:r) i = if i == 0
    then Just a
    else index r (i - 1)
  
instance Container Set where
  type Elem Set a = Ord a
  
  empty = Set.empty
  null = Set.null
  singleton = Set.singleton
  concat = Set.unions
  size = Set.size
  
  fromList = Set.fromList
  toList = Set.toList
  
  mapMaybe f = fromList . May.mapMaybe f . toList
  
  map = Set.map
  filter = Set.filter
  
concatMap :: (Container co, Elem co a, Elem co b) => (a -> co b) -> co a -> co b
concatMap f = concat . Pre.map f . toList