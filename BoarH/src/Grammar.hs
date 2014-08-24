{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Grammar where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import MultiMap (MultiMap)
import qualified MultiMap as MM
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Control.Arrow (second)
import Data.Monoid (Monoid(..))
import Debug.Trace (trace)

type Prod = []

data Grammar a = Grammar
  { terms :: Set a
  , rules :: Map a [Prod a]
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
  
-- Small Helpers

mergeSets :: Ord a => Set (Set a) -> Set a
mergeSets = S.unions . S.toList

mapRemoveKeys :: Ord a => Set a -> Map a v -> Map a v
mapRemoveKeys ks = M.filterWithKey (\ k _ -> not (S.member k ks))

disjoint :: Ord a => Set a -> Set a -> Bool
disjoint a b = S.null $ a `S.intersection` b

toElemMap :: (Ord k, Ord v) => [(k, v)] -> Map k (Set v)
toElemMap = M.fromListWith S.union . map (second S.singleton)

setMapMaybe :: (Ord a, Ord b) => (a -> Maybe b) -> Set a -> Set b
setMapMaybe f = S.fromList . mapMaybe f . S.toList
  
-- | Check that a grammar is valid.
valid :: Ord a => Grammar a -> Bool
valid g = terms g `disjoint` nterms g &&
          all (all (all (iselem g))) (M.elems $ rules g)
  
makeGrammar :: Ord a => Set a -> Map a [Prod a] -> Maybe (Grammar a)
makeGrammar ts rs =
  let g = Grammar ts rs
  in if valid g
     then Just g
     else Nothing

fixpoint :: (a -> Maybe a) -> a -> a
fixpoint f a = case f a of
  Nothing -> a
  Just a' -> fixpoint f a'

fixpointEq :: Eq a => (a -> a) -> a -> a
fixpointEq f = fixpoint (\x -> let x' = f x in if x == x' then Nothing else Just x')

fixpointSet :: Ord a => (a -> Set a) -> Set a -> Set a
fixpointSet f i = go i i
  where
    go currSet nextSet =
      let newSets = S.map f nextSet
          newItems = mergeSets newSets
      in go (newItems `S.union` currSet) (newItems `S.difference` currSet)

constMap :: Ord k => v -> Set k -> Map k v      
constMap v = M.fromSet (const v)

fixpointPostMap :: (Ord k, Ord v) => (MultiMap k v -> k -> Set v) -> MultiMap k v -> MultiMap k v
fixpointPostMap f = fixpointEq iter
  where
    iter m = m `MM.union` MM.mapValuesWith (\k _ -> f m k) m

fixpointMap :: (Ord k, Ord v) => (v -> Set v) -> MultiMap k v -> MultiMap k v
fixpointMap f = fixpointEq iter
  where
    iter m = m `MM.union` MM.mapValues f m
    
    
    
nullables :: Ord a => Grammar a -> Set a
nullables g = fixpointEq nextNullables S.empty
  where
    nextNullables ns = S.union ns ns'
      where
        isNullable = all (`S.member` ns)
        hasNullableRule = any isNullable
        removeKeys = mapRemoveKeys ns
        
        nonRules = removeKeys (rules g)
        nullableMap = M.filter hasNullableRule nonRules
        ns' = S.fromList $ M.keys nullableMap
      
      
firsts :: forall a . (Show a, Ord a) => Grammar a -> MultiMap a a
firsts g = trace (show termFirsts) $ MM.transitiveClosure allPossibleFirsts
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
      
data FollowItem a = NextItem a
                  | EndItemParent a
  deriving (Eq, Ord, Show)
  
aggregate :: Ord a => (b -> c) -> (c -> c -> c) -> (c -> d) -> [(a, b)] -> Map a d
aggregate intro combine extro l =
  M.map extro $
  M.fromListWith combine $
  map (second intro) l
  
createPairs :: (Ord a, Ord b) => b -> Set a -> MultiMap a b
createPairs b s = MM.fromList $ map (\x -> (x, b)) $ S.toList s

unwrap :: (a -> a -> a) -> (b -> b -> b) -> (a, b) -> [(a, b)] -> (a, b)
unwrap fa fb
  = foldr (\(as', bs') (as, bs) -> (fa as' as, fb bs' bs))
  
unwrapMonoid :: (Monoid a, Monoid b) => [(a, b)] -> (a, b)
unwrapMonoid = unwrap mappend mappend (mempty, mempty)
  
unwrapLists :: [([a], [b])] -> ([a], [b])
unwrapLists = unwrap (++) (++) ([], [])

follows :: forall a . (Show a, Ord a) => Grammar a -> MultiMap a a
follows g = fixp
  where
    nullableSet = nullables g
    firstSets = firsts g
    
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
                      prevAdj' = S.singleton a `S.union` if a `S.member` nullableSet then prevAdj else S.empty
                   in go prevAdj' inc' r
      
    ntermFollows :: [Prod a] -> (MultiMap a a, Set a)
    ntermFollows ps = unwrapMonoid $ map adjacentPairs ps
    
    (allAdjs, allEnds) = M.foldWithKey
      (\nt ps (adjs, ends) ->
         let (adjs', end') = ntermFollows ps
         in (adjs' `mappend` adjs, createPairs nt end' `mappend` ends))
      (mempty, mempty)
      (rules g)
      
    base :: MultiMap a a
    base = MM.mapValues (firstSets MM.!) allAdjs
    
    nextFollows :: MultiMap a a -> MultiMap a a
    nextFollows m =
      m `mappend` MM.mapValues (allEnds MM.!) m
      
    fixp = fixpointEq nextFollows base

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