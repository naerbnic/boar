module Boar.Base.Rule
  ( Rule(..)
  , isNullable
  , AdjResult(..)
  , adjacent
  , allElems
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

infix 8 :=>
data Rule a = (:=>)
  { lhs :: a
  , rhs :: [a]
  } deriving (Eq, Ord)
  
instance Show a => Show (Rule a) where
  show (lh :=> rh) = unwords $ [show lh, ":=>"] ++ map show rh
  
instance Functor Rule where
  fmap f (lh :=> rh) = f lh :=> map f rh
  
isNullable :: Ord a => Set a -> Rule a -> Bool
isNullable nullables (_ :=> rh) = all (`Set.member` nullables) rh

data AdjResult a
  = AtStart a
  | Adjacent a a
  | AtEnd a
  deriving (Eq, Ord, Show)
  
adjacent :: Ord a => Set a -> Rule a -> [AdjResult a]
adjacent nullables (_ :=> rh) = let
  go [] = ([], [])
  go (a:r) = let
    (curr, collection) = go r
    in if a `Set.member` nullables
       then (a:curr, collection)
       else ([a], (a:curr):collection)
  
  -- Runs of elements which are nullable, beginning and ending with a
  -- non-nullable symbol, or the beginning or end of the rule.
  runs = let
    (curr, collection) = go rh
    in if null curr
      then collection
      else curr:collection
  
  in if null runs
     then []
     else let
       first = head runs
       final = last runs
       
       pairs [] = []
       pairs (a:r) = map (Adjacent a) r ++ pairs r
       
       middle = concatMap pairs runs
       in map AtStart first ++ middle ++ map AtEnd final
       
allElems :: Ord a => Rule a -> Set a
allElems (lh :=> rh) = Set.singleton lh `Set.union` Set.fromList rh