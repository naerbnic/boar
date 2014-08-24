{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Grammar where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import MultiMap (MultiMap)
import qualified MultiMap as MM
import Data.Maybe (fromJust)
import Data.Monoid (Monoid(..))

type Prod = []

data Grammar a = Grammar
  { terms :: Set a
  , rules :: Map a [Prod a]
  
  -- Calculated lazily from create*
  , nullables :: Set a
  , firsts :: MultiMap a a
  , follows :: MultiMap a a
  } deriving (Eq, Show)
  
isterm :: Ord a => Grammar a -> a -> Bool
isterm g = (`S.member` terms g) 
  
nterms :: Ord a => Grammar a -> Set a
nterms g = S.fromList (M.keys $ rules g)

isnterm :: Ord a => Grammar a -> a -> Bool
isnterm g = (`S.member` nterms g)

elems :: Ord a => Grammar a -> Set a
elems g = terms g `S.union` nterms g

iselem :: Ord a => Grammar a -> a -> Bool
iselem g = (`S.member` elems g)

isNullable :: Ord a => Grammar a -> a -> Bool
isNullable g = (`S.member` nullables g)

getFirsts :: Ord a => Grammar a -> a -> Set a
getFirsts g = (firsts g MM.!)

getFollows :: Ord a => Grammar a -> a -> Set a
getFollows g = (follows g MM.!)
  
-- Small Helpers

mapRemoveKeys :: Ord a => Set a -> Map a v -> Map a v
mapRemoveKeys ks = M.filterWithKey (\ k _ -> not (S.member k ks))

disjoint :: Ord a => Set a -> Set a -> Bool
disjoint a b = S.null $ a `S.intersection` b

-- | Check that a grammar is valid.
valid :: Ord a => Grammar a -> Bool
valid g = terms g `disjoint` nterms g &&
          all (all (all (iselem g))) (M.elems $ rules g)
  
makeGrammar :: Ord a => Set a -> Map a [Prod a] -> Maybe (Grammar a)
makeGrammar ts rs =
  let g = Grammar ts rs (createNullables g) (createFirsts g) (createFollows g)
  in if valid g
     then Just g
     else Nothing

fixpoint :: (a -> Maybe a) -> a -> a
fixpoint f a = case f a of
  Nothing -> a
  Just a' -> fixpoint f a'

fixpointEq :: Eq a => (a -> a) -> a -> a
fixpointEq f = fixpoint (\x -> let x' = f x in if x == x' then Nothing else Just x')
    
createNullables :: Ord a => Grammar a -> Set a
createNullables g = fixpointEq nextNullables S.empty
  where
    nextNullables ns = S.union ns ns'
      where
        areNullable = all (`S.member` ns)
        hasNullableRule = any areNullable
        removeKeys = mapRemoveKeys ns
        
        nonRules = removeKeys (rules g)
        nullableMap = M.filter hasNullableRule nonRules
        ns' = S.fromList $ M.keys nullableMap
      
      
createFirsts :: forall a . Ord a => Grammar a -> MultiMap a a
createFirsts g = MM.transitiveClosure allPossibleFirsts
  where
    nullableSet = nullables g
    
    possibleFirsts :: Prod a -> Set a
    possibleFirsts [] = S.empty
    possibleFirsts (a:r) =
      S.singleton a `S.union`
        if a `S.member` nullableSet then possibleFirsts r else S.empty
        
    ntermPossibleFirsts ps = S.unions $ map possibleFirsts ps
    
    termFirsts = MM.fromSet S.singleton (terms g)
    ntermFirsts = MM.fromSet (\nt -> ntermPossibleFirsts (rules g M.! nt)) (nterms g)
     
    allPossibleFirsts = termFirsts `MM.union` ntermFirsts
  
createPairs :: (Ord a, Ord b) => b -> Set a -> MultiMap a b
createPairs b s = MM.fromList $ map (\x -> (x, b)) $ S.toList s

unwrap :: (a -> a -> a) -> (b -> b -> b) -> (a, b) -> [(a, b)] -> (a, b)
unwrap fa fb
  = foldr (\(as', bs') (as, bs) -> (fa as' as, fb bs' bs))
  
unwrapMonoid :: (Monoid a, Monoid b) => [(a, b)] -> (a, b)
unwrapMonoid = unwrap mappend mappend (mempty, mempty)

createFollows :: forall a . Ord a => Grammar a -> MultiMap a a
createFollows g = fixpointEq nextFollows base
  where
    -- | `adjacentPairs nullables prod -> (adjacent, end)`, where nullables is a set
    -- nullable elements, prod is a single production, adjacent is an assoc list of
    -- adjacent elements, and end is a list of elements adjacent to the end.
    adjacentPairs :: Prod a -> (MultiMap a a, Set a)
    adjacentPairs = go S.empty MM.empty
      where
        go prevAdj inc l = case l of
          [] -> (inc, prevAdj)
          a : r -> let
                      inc' = inc `MM.union` createPairs a prevAdj
                      prevAdj' = S.singleton a `S.union` if isNullable g a then prevAdj else S.empty
                   in go prevAdj' inc' r
      
    ntermFollows :: [Prod a] -> (MultiMap a a, Set a)
    ntermFollows ps = unwrapMonoid $ map adjacentPairs ps
    
    -- allAdjs: elem -> adjacent elem
    -- allEnds: elem -> nterm which ends with the elem
    (allAdjs, allEnds) = M.foldWithKey
      (\nt ps (adjs, ends) ->
         let (adjs', end') = ntermFollows ps
         in (adjs' `mappend` adjs, createPairs nt end' `mappend` ends))
      (mempty, mempty)
      (rules g)
      
    base :: MultiMap a a
    base = allAdjs `mappend` MM.mapValues (getFirsts g) allAdjs
    
    nextFollows :: MultiMap a a -> MultiMap a a
    nextFollows m = m `mappend` MM.mapValues (m MM.!) allEnds

gram1 :: Grammar String
gram1 = fromJust $ makeGrammar
    (S.fromList ["a", "b"])
    (M.fromList [("e", [[], 
                        ["e", "a", "e", "b"],
                        ["b", "e", "a"]]),
                 ("s", [["e"]])])
                 
gram2 :: Grammar String
gram2 = fromJust $ makeGrammar
    (S.fromList ["a", "b", "c"])
    (M.fromList [("x", [[], ["a"]]),
                 ("y", [[], ["b"]]),
                 ("z", [[], ["c"]]),
                 ("s", [["x", "y", "z"]])])