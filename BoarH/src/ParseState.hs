module ParseState
  ( State
  , stateNexts
  , expandProdStateNullable
  , expandProdStateNT
  , expandClosure
  , initialState
  , createLR0States
  ) where

import           Boar.Data.Graph    (Graph, Node (..))
import qualified Boar.Data.Graph    as GR
import qualified Boar.Data.MultiMap as MM
import           Boar.Util.Fixpoint
import qualified Data.Map           as M
import           Data.Maybe         (catMaybes)
import           Data.Set           (Set)
import qualified Data.Set           as S
import           Data.Tuple         (swap)
import           Grammar            hiding (start)
import qualified Grammar            as G
import           ProdState

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

-- | A parse state defined as a collection of production states.
type State a = Set (ProdState a)

stateNexts :: Ord a => State a -> [(State a, a)]
stateNexts st = map swap $ M.toList $ MM.toMap $ MM.from $ mapMaybeSet step st

expandNTerm :: Ord a => Grammar a -> a -> State a
expandNTerm g nt = S.map start $ ntermRules g nt

initialState :: Ord a => Grammar a -> State a
initialState g = expandClosure g (expandNTerm g (G.start g))

expandProdStateNT :: Ord a => Grammar a -> ProdState a -> State a
expandProdStateNT g ps = case atPoint ps of
  Just nt | isnterm g nt -> expandNTerm g nt
  _ -> S.empty

expandProdStateNullable :: Ord a => Grammar a -> ProdState a -> State a
expandProdStateNullable g ps = maybeToSet $ do
  pt <- atPoint ps
  if pt `S.member` nullables g
    then inc ps
    else Nothing

expandClosure :: Ord a => Grammar a -> State a -> State a
expandClosure g = fixpointSet
  (mergeSetFunctions [expandProdStateNT g, expandProdStateNullable g])

createLR0States :: Ord a => Grammar a -> Graph (State (FullElem a)) () (Maybe a)
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
