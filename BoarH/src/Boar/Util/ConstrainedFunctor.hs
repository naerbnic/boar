{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Boar.Util.ConstrainedFunctor where

import GHC.Exts (Constraint)
import Prelude hiding (concat)
import qualified Prelude as Pre
import qualified Data.Maybe as May
import Data.Set (Set)
import qualified Data.Set as Set
  
class Container co where
  type ValidElem co a :: Constraint
  
  empty :: ValidElem co a => co a
  null :: ValidElem co a => co a -> Bool
  singleton :: ValidElem co a => a -> co a
  concat :: ValidElem co a => [co a] -> co a
  
  fromList :: ValidElem co a => [a] -> co a
  fromList = concat . Pre.map singleton
  
  toList :: ValidElem co a => co a -> [a]
  
  mapMaybe :: (ValidElem co a, ValidElem co b) => (a -> Maybe b) -> co a -> co b
  
  map :: (ValidElem co a, ValidElem co b) => (a -> b) -> co a -> co b
  map f = mapMaybe (Just . f)
  
  filter :: (ValidElem co a) => (a -> Bool) -> co a -> co a
  filter f = mapMaybe (\x -> if f x then Just x else Nothing)
  
instance Container [] where
  type ValidElem [] a = ()
  
  empty = []
  null = Pre.null
  singleton x = [x]
  concat = Pre.concat
  
  fromList = id
  toList = id
  
  mapMaybe = May.mapMaybe
  map = Pre.map
  filter = Pre.filter
  
instance Container Set where
  type ValidElem Set a = Ord a
  
  empty = Set.empty
  null = Set.null
  singleton = Set.singleton
  concat = Set.unions
  
  fromList = Set.fromList
  toList = Set.toList
  
  mapMaybe f = fromList . May.mapMaybe f . toList
  
  map = Set.map
  filter = Set.filter
  
concatMap :: (Container co, ValidElem co a, ValidElem co b) => (a -> co b) -> co a -> co b
concatMap f = concat . Pre.map f . toList