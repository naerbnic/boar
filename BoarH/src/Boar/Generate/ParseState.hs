module Boar.Generate.ParseState
  ( ParseState
  , stateNexts
  {-
  , expandProdStateNullable
  , expandProdStateNT
  , expandClosure
  , initialState
  , createLR0States
  -}
  ) where

import           Boar.Data.Graph    (Graph, Node (..))
import qualified Boar.Data.Graph    as GR
import qualified Boar.Data.MultiMap as MM
import           Boar.Util.Fixpoint
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Maybe         (catMaybes)
import           Data.Set           (Set)
import qualified Data.Set           as S
import           Data.Tuple         (swap)
import           Boar.Base.Grammar            hiding (start)
import qualified Boar.Base.Grammar            as G
import           Boar.Generate.ProdState

-- Helpers 

maybeToSet :: Maybe a -> Set a
maybeToSet Nothing = S.empty
maybeToSet (Just a) = S.singleton a

catMaybeSet :: Ord a => Set (Maybe a) -> Set a
catMaybeSet = S.fromList . catMaybes . S.toList

mapMaybeSet :: Ord b => (a -> Maybe b) -> Set a -> Set b
mapMaybeSet f s = catMaybeSet $ S.map f s

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)

recombine :: (Ord a, Ord b) => Set (a, b) -> Map a (Set b)
recombine = MM.toMap . MM.from

-- | A parse state defined as a collection of production states.
newtype ParseState a = ParseState { prodStates :: Set (ProdState a) }
  deriving (Eq, Ord, Show)

stateNexts :: Ord a => ParseState a -> [(ParseState a, a)]
stateNexts (ParseState st) = let
  rulePairs = mapMaybeSet step st
  recombinedSets = recombine rulePairs
  recombinedStates = M.map ParseState recombinedSets
  in map swap $ M.toList recombinedStates
  
nextElems :: Ord a => ParseState a -> Set a
nextElems ps = S.fromList $ map snd $ stateNexts ps

expandNTerm :: Ord a => Grammar a -> a -> ParseState a
expandNTerm g nt = ParseState $ S.map start $ ntermRules g nt

{-
initialState :: Ord a => Grammar a -> ParseState a
initialState g = expandClosure g (expandNTerm g (G.start g))


expandProdStateNT :: Ord a => Grammar a -> ProdState a -> ParseState a
expandProdStateNT g ps = case atPoint ps of
  Just nt | isnterm g nt -> expandNTerm g nt
  _ -> S.empty

expandProdStateNullable :: Ord a => Grammar a -> ProdState a -> ParseState a
expandProdStateNullable g ps = maybeToSet $ do
  pt <- atPoint ps
  if pt `S.member` nullables g
    then inc ps
    else Nothing

expandClosure :: Ord a => Grammar a -> ParseState a -> ParseState a
expandClosure g = fixpointSet
  (mergeSetFunctions [expandProdStateNT g, expandProdStateNullable g])

createLR0States :: Ord a => Grammar a -> Graph (ParseState (FullElem a)) () (Maybe a)
createLR0States g = GR.unfold
    traverse
    GR.combineEq
    GR.combineEq
    (Node (initialState fullGrammar) ())
  where
    fullGrammar = createFullGrammar g
    traverse (Node s ()) = let
      nexts = map (mapFst (expandClosure fullGrammar)) $ stateNexts s

      pairToEdge (s', Elem a) = (Node s' (), a)
      pairToEdge (_, Start) = error "Unexpected start symbol in rule"

      in map pairToEdge nexts
-}