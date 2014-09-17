{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Grammar where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import MultiMap (MultiMap)
import qualified MultiMap as MM
import Data.Maybe (fromJust)
import Data.Monoid (Monoid(..), (<>))
import Fixpoint

type Prod = []

-- Rule definition
--

data Rule a = Rule
  { lhs :: a
  , rhs :: Prod a
  } deriving (Eq, Ord)
  
instance Show a => Show (Rule a) where
  show (Rule l r) = 
    show l ++ " -> " ++ unwords (map show r) 

data Grammar a = Grammar
  { terms :: Set a
  , ruleMap :: Map a [Prod a]
  
  -- Calculated lazily from create*
  , nullables :: Set a
  , firsts :: MultiMap a a
  , follows :: MultiMap a a
  
  } deriving (Eq, Show)
  
isterm :: Ord a => Grammar a -> a -> Bool
isterm g = (`S.member` terms g) 
  
nterms :: Ord a => Grammar a -> Set a
nterms g = S.fromList (M.keys $ ruleMap g)

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

rules :: Ord a => Grammar a -> [Rule a]
rules g = concatMap
  (\ (parent, prods) -> map (Rule parent) prods)
  (M.toList (ruleMap g))
  
ntermRules :: Ord a => Grammar a -> a -> [Rule a]
ntermRules g nt = map (Rule nt) $ ruleMap g M.! nt
  
-- Small Helpers

mapRemoveKeys :: Ord a => Set a -> Map a v -> Map a v
mapRemoveKeys ks = M.filterWithKey (\ k _ -> not (S.member k ks))

disjoint :: Ord a => Set a -> Set a -> Bool
disjoint a b = S.null $ a `S.intersection` b

-- | Check that a grammar is valid.
valid :: Ord a => Grammar a -> Bool
valid g = terms g `disjoint` nterms g &&
          all (all (all (iselem g))) (M.elems $ ruleMap g)
  
makeGrammar :: Ord a => Set a -> Map a [Prod a] -> Maybe (Grammar a)
makeGrammar ts rs =
  let g = Grammar ts rs (createNullables g) (createFirsts g) (createFollows g)
  in if valid g
     then Just g
     else Nothing
    
createNullables :: Ord a => Grammar a -> Set a
createNullables g = fixpointEq nextNullables S.empty
  where
    nextNullables ns = S.union ns ns'
      where
        areNullable = all (`S.member` ns)
        hasNullableRule = any areNullable
        removeKeys = mapRemoveKeys ns
        
        nonRules = removeKeys (ruleMap g)
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
    ntermFirsts = MM.fromSet (\nt -> ntermPossibleFirsts (ruleMap g M.! nt)) (nterms g)
     
    allPossibleFirsts = termFirsts `MM.union` ntermFirsts
  
createPairs :: (Ord a, Ord b) => b -> Set a -> MultiMap a b
createPairs b s = MM.fromList $ map (\x -> (x, b)) $ S.toList s

unwrap :: (Monoid a, Monoid b) => [(a, b)] -> (a, b)
unwrap = foldr (\(as', bs') (as, bs) -> (as' <> as, bs' <> bs)) (mempty, mempty)

createFollows :: forall a . Ord a => Grammar a -> MultiMap a a
createFollows g = fixpointEq iter base
  where
    adjacentPairs (Rule parent p) = go S.empty MM.empty p
      where
        go prevAdj inc l = case l of
          [] -> (inc, createPairs parent prevAdj)
          a : r ->
            let nextAdj = if isNullable g a
                  then prevAdj
                  else S.empty
                inc' = inc `MM.union` createPairs a prevAdj
                prevAdj' = S.singleton a `S.union` nextAdj
            in go prevAdj' inc' r
    
    -- allAdjs: elem -> adjacent elem
    -- allEnds: elem -> nterm which ends with the elem
    (allAdjs, allEnds) = unwrap $ map adjacentPairs $ rules g
      
    base = allAdjs <> MM.mapValues (getFirsts g) allAdjs
    
    iter m = m <> MM.mapValues (m MM.!) allEnds
    
data FullElem a
  = Start
  | Elem (Maybe a)
  deriving (Ord, Eq, Show)
    
createFullGrammar :: Ord a => Grammar a -> a -> Grammar (FullElem a)
createFullGrammar g start = let
    newProd = [Elem $ Just start, Elem Nothing]
    newTerms = S.map (Elem . Just) (terms g) `S.union` S.singleton (Elem Nothing)
    newRules =
      M.mapKeys (Elem . Just) $
      M.map (map $ map (Elem . Just)) $
      ruleMap g
    
    in fromJust $ makeGrammar newTerms (newRules `M.union` M.singleton Start [newProd])

-- Examples

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
                 
gram3 :: Grammar String
gram3 = fromJust $ makeGrammar
    (S.fromList ["(", "a", "+", ")"])
    (M.fromList [("e", [["a"],
                        ["e", "+", "e"],
                        ["(", "e", ")"]]),
                 ("s", [["s"]])])