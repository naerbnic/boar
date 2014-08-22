{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Grammar where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Control.Arrow (second)

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

fixpointPostMap :: (Ord k, Ord v) => (Map k (Set v) -> k -> Set v) -> Map k (Set v) -> Map k (Set v)
fixpointPostMap f = fixpointEq iter
  where
    iter m = M.mapWithKey (\k v -> v `S.union` f m k) m
    
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
      
      
firsts :: forall a . Ord a => Grammar a -> Map a (Set a)
firsts g = fixpointPostMap nextFirst (constMap S.empty (nterms g))
  where
    nullableSet = nullables g
   
    getFirst :: Map a (Set a) -> a -> Set a
    getFirst m a = if isterm g a
      then S.singleton a
      else m M.! a
    
    prodFirst :: Map a (Set a) -> Prod a -> Set a
    prodFirst m l = case l of
      [] -> S.empty
      a:r -> let currFirst = getFirst m a
             in if a `S.member` nullableSet
               then currFirst `S.union` prodFirst m r
               else currFirst
               
    listFirsts :: Map a (Set a) -> [Prod a] -> Set a
    listFirsts m s = S.unions (map (prodFirst m) s) 
      
    nextFirst :: Map a (Set a) -> a -> Set a
    nextFirst m a = 
      let arules = rules g M.! a
      in listFirsts m arules
      
data FollowItem a = NextItem a
                  | EndItemParent a
  deriving (Eq, Ord, Show)

follows :: forall a . Ord a => Grammar a -> Map a (Set a)
follows g = M.map (setMapMaybe item2Maybe) fixp
  where
    nullableSet = nullables g
    firstSets = firsts g
    
    headsWithNullables :: [a] -> [Maybe a]
    headsWithNullables [] = [Nothing]
    headsWithNullables (a : r) = Just a : if a `S.member` nullableSet
                                            then headsWithNullables r
                                            else []
    
    prodFollows :: a -> Prod a -> [(a, FollowItem a)]
    prodFollows _ [] = []
    prodFollows parent (a : r) = 
      let restFollows = prodFollows parent r
          prodItems = map (maybe (EndItemParent parent) NextItem) (headsWithNullables r)
      in map (\x -> (a, x)) prodItems ++ restFollows
    
    -- The follows relation which will be iterated with firsts to find a fixed point
    baseFollows :: Map a (Set (FollowItem a))
    baseFollows = toElemMap $ M.toList (rules g) >>= (\(parent, lst) -> lst >>= prodFollows parent)
    
    baseFollows' = baseFollows `M.union` constMap S.empty (elems g `S.difference` S.fromList (M.keys baseFollows))
    
    nextFollows :: Map a (Set (FollowItem a)) -> a -> Set (FollowItem a)
    nextFollows m k =
      let currSet = m M.! k
          sets = map (\x ->
            case x of
              EndItemParent p -> m M.! p
              NextItem k' -> S.map NextItem $ fromMaybe S.empty (M.lookup k' firstSets))
            (S.toList currSet) 
      in S.unions sets
      
    fixp = fixpointPostMap nextFollows baseFollows'
    
    item2Maybe it = case it of
      EndItemParent _ -> Nothing
      NextItem a -> Just a
    
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