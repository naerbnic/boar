{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Boar.Base.Grammar where

import           Boar.Base.Rule     (Rule(..))
import qualified Boar.Base.Rule     as Rule
import           Boar.Util.Fixpoint
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Maybe         (fromJust)
import           Data.Set           (Set)
import qualified Data.Set           as S

data Grammar a = Grammar
  { terms     :: Set a
  , rules     :: Set (Rule a)
  , start     :: a

  -- Calculated lazily from create*
  , nullables :: Set a

  } deriving (Eq, Show)

isterm :: Ord a => Grammar a -> a -> Bool
isterm g = (`S.member` terms g)

nterms :: Ord a => Grammar a -> Set a
nterms g = S.map Rule.lhs $ rules g

isnterm :: Ord a => Grammar a -> a -> Bool
isnterm g = (`S.member` nterms g)

elems :: Ord a => Grammar a -> Set a
elems g = terms g `S.union` nterms g

iselem :: Ord a => Grammar a -> a -> Bool
iselem g = (`S.member` elems g)

isNullable :: Ord a => Grammar a -> a -> Bool
isNullable g = (`S.member` nullables g)

ntermRules :: Ord a => Grammar a -> a -> Set (Rule a)
ntermRules g nt = S.filter (\r -> Rule.lhs r == nt) $ rules g

-- Small Helpers

mapRemoveKeys :: Ord a => Set a -> Map a v -> Map a v
mapRemoveKeys ks = M.filterWithKey (\ k _ -> not (S.member k ks))

disjoint :: Ord a => Set a -> Set a -> Bool
disjoint a b = S.null $ a `S.intersection` b

-- | Check that a grammar is valid.
valid :: Ord a => Grammar a -> Bool
valid g = terms g `disjoint` nterms g

makeGrammar :: Ord a => Set a -> Set (Rule a) -> a -> Maybe (Grammar a)
makeGrammar ts rs st =
  let g = Grammar ts rs st (createNullables g)
  in if valid g
     then Just g
     else Nothing

createNullables :: Ord a => Grammar a -> Set a
createNullables g = fixpointEq nextNullables S.empty
  where
    nextNullables ns =
      S.map Rule.lhs $
      S.filter (Rule.isNullable ns) (rules g)

data FullElem a
  = Start
  | Elem (Maybe a)
  deriving (Ord, Eq)

instance Show a => Show (FullElem a) where
  show Start = "START"
  show (Elem (Just a)) = show a
  show (Elem Nothing) = "$"

createFullGrammar :: Ord a => Grammar a -> Grammar (FullElem a)
createFullGrammar g = let
    newProd = [Elem $ Just (start g), Elem Nothing]
    newTerms = S.map (Elem . Just) (terms g) `S.union` S.singleton (Elem Nothing)
    newRules =
      S.map (fmap (Elem . Just)) (rules g)

    in fromJust $ makeGrammar
      newTerms
      (newRules `S.union` S.singleton (Start :=> newProd))
      Start

-- Examples

gram1 :: Grammar String
gram1 = fromJust $ makeGrammar
    (S.fromList ["A", "B"])
    (S.fromList ["e" :=> [],
                 "e" :=> ["e", "A", "e", "B"],
                 "e" :=> ["B", "e", "A"],
                 "s" :=> ["e"]])
    "s"

gram2 :: Grammar String
gram2 = fromJust $ makeGrammar
    (S.fromList ["a", "b", "c"])
    (S.fromList ["x" :=> [], "x" :=> ["a"],
                 "y" :=> [], "y" :=> ["b"],
                 "z" :=> [], "z" :=> ["c"],
                 "s" :=> ["x", "y", "z"]])
    "s"

gram3 :: Grammar String
gram3 = fromJust $ makeGrammar
    (S.fromList ["(", "a", "+", ")"])
    (S.fromList ["e" :=> ["a"],
                 "e" :=> ["e", "+", "e"],
                 "e" :=> ["(", "e", ")"],
                 "s" :=> ["s"]])
    "s"
