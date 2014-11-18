module Generate.EarleyStates where

import           Data.Foldable (foldMap)
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Data.Tuple    (swap)
import           Boar.Util.Fixpoint
import           Grammar
import           Boar.Data.MultiMap      (MultiMap)
import qualified Boar.Data.MultiMap      as MM
import           ParseState
import qualified ProdState     as PS

-- Utilities

setToMap :: Ord a => (a -> b) -> Set a -> Map a b
setToMap f s = M.fromList $ map (\x -> (x, f x)) $ S.toList s

combiner :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
combiner c f1 f2 x = c (f1 x) (f2 x)

{-|
When we transition from an Earley state (st, i), the current Earley set will
end up holding (incState st, i) and (freshState, j), where j is the origin of
the current Earley set, if freshState exists. For example, if st is:

a -> . FOO b

then on transition of FOO, incState would be

a -> FOO . b

while freshState would be:

b -> . c BAR
c -> . BAZ

assuming the appropriate rules in the grammar.
-}
data ResultStates k = ResultStates
  { incState   :: k
  , freshState :: Maybe k
  } deriving (Eq, Ord, Show)

{-|
For a given Earley state, this shows which nonterminals are complete in this
state, as well as what transitions are available, and their resulting
result states.
-}
data EarleyInfo k a = EarleyInfo
  { complete    :: MultiMap a (Rule a)
  , transitions :: Map a (ResultStates k)
  } deriving (Eq, Ord, Show)

{-|

-}
data StateCollection k a = StateCollection
  { startState :: k
  , states     :: Map k (EarleyInfo k a)
  } deriving (Eq, Ord, Show)

-- | Creates a result state given an incomplete transition state, such as
-- the result of stateNexts.
nextToResultStates :: Ord a => Grammar a -> State a -> ResultStates (State a)
nextToResultStates g st = let
  -- Nullable expand everything in this state, as those will all share the
  -- same origin
  currClosure = fixpointSet (expandProdStateNullable g) st

  -- Seed for those things that will have a new origin
  nextSeed = foldMap (expandProdStateNT g) currClosure

  -- Perform a closure on the seed to create the set of all things that
  -- has the new origin
  freshSet = expandClosure g nextSeed
  in ResultStates currClosure
                  (if S.null freshSet then Nothing else Just freshSet)

-- | Creates the Earley info for a parse state
stateTransitions :: Ord a => Grammar a -> State a -> Map a (ResultStates (State a))
stateTransitions g st = let
  nexts = stateNexts st
  in M.map (nextToResultStates g) $ M.fromList (map swap nexts)

incStates :: (Ord a, Ord k) => Map k (Map a (ResultStates k)) -> Set k
incStates trans = S.fromList $ map incState $ concatMap M.elems (M.elems trans)

-- | Returns all parse states inside the Earley info.
transStates :: (Ord a, Ord k) => Map a (ResultStates k) -> Set k
transStates m = let
  results = M.elems m

  resultStates (ResultStates i f) = S.singleton i `S.union` case f of
    Nothing -> S.empty
    Just st -> S.singleton st
  in S.unions $ map resultStates results

earleyStates :: Ord a => Grammar a -> Set (State a)
earleyStates g = let
  expandedInitialState = expandClosure g (initialState g)
  in fixpointSet (transStates . stateTransitions g)
                 (S.singleton expandedInitialState)

earleyStateCollection :: Ord a => Grammar a -> StateCollection (State a) a
earleyStateCollection g = let
  stateTransitionsMap = setToMap (stateTransitions g) (earleyStates g)
  incStatesSet = incStates stateTransitionsMap

  makeInfo k v = let
    completeSet = if k `S.member` incStatesSet
      then S.map PS.rule $ S.filter PS.complete k
      else S.empty

    in EarleyInfo (MM.groupValues lhs completeSet) v

  statesMap = M.mapWithKey makeInfo stateTransitionsMap

  in StateCollection (initialState g) statesMap
