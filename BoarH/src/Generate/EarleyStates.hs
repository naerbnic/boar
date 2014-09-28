module Generate.EarleyStates where

import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Set   (Set)
import qualified Data.Set   as S
import           ParseState
import Grammar
import Fixpoint
import Data.Foldable (foldMap)
import qualified ProdState as PS
import Data.Tuple (swap)

-- Utilities

setToMap :: Ord a => (a -> b) -> Set a -> Map a b
setToMap f s = M.fromList $ map (\x -> (x, f x)) $ S.toList s

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
data ResultStates a = ResultStates
  { incState   :: State a
  , freshState :: Maybe (State a)
  } deriving (Eq, Ord, Show)

{-|
For a given Earley state, this shows which nonterminals are complete in this
state, as well as what transitions are available, and their resulting 
result states.
-}
data EarleyInfo a = EarleyInfo
  { complete    :: Set a
  , transitions :: Map a (ResultStates a)
  } deriving (Eq, Ord, Show)

{-|

-}
data StateCollection a = StateCollection
  { startState :: State a
  , states     :: Map (State a) (EarleyInfo a)
  } deriving (Eq, Ord, Show)

-- | Creates a result state given an incomplete transition state, such as
-- the result of stateNexts.
nextToResultStates :: Ord a => Grammar a -> State a -> ResultStates a
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
stateEarleyInfo :: Ord a => Grammar a -> State a -> EarleyInfo a
stateEarleyInfo g st = let
  nexts = stateNexts st
  completeSet = S.map PS.lhs $ S.filter PS.complete st
  
  transitionMap = M.map (nextToResultStates g) $ M.fromList (map swap nexts)
  in EarleyInfo completeSet transitionMap
  
-- | Returns all parse states inside the Earley info.
infoStates :: Ord a => EarleyInfo a -> Set (State a)
infoStates ei = let
  results = M.elems (transitions ei)
  
  resultStates (ResultStates i f) = S.singleton i `S.union` case f of
    Nothing -> S.empty
    Just st -> S.singleton st
  in S.unions $ map resultStates results
  
earleyStates :: Ord a => Grammar a -> Set (State a)
earleyStates g = let
  expandedInitialState = expandClosure g (initialState g)
  in fixpointSet (infoStates . stateEarleyInfo g)
                 (S.singleton expandedInitialState)
                 
earleyStateCollection :: Ord a => Grammar a -> StateCollection a
earleyStateCollection g = let
  statesMap = setToMap (stateEarleyInfo g) (earleyStates g)
  in StateCollection (initialState g) statesMap