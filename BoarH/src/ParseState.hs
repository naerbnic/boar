module ParseState where

import           Data.Maybe (fromJust, mapMaybe)
import           Data.Set   (Set)
import qualified Data.Set   as S
import           Fixpoint
import           Grammar    hiding (lhs, start)
import qualified Grammar    as G
import           Graph      (Graph, Node (..))
import qualified Graph      as GR
import           ProdState

-- | A parse state defined as a collection of production states.
type State a = Set (ProdState a)

nextElemsInState :: Ord a => State a -> Set a
nextElemsInState st = S.fromList $ mapMaybe atPoint $ S.toList st

nextStateForElem :: Ord a => a -> State a -> Maybe (State a)
nextStateForElem el st = let
  prodSet = S.fromList $ mapMaybe (next el) $ S.toList st
  in if S.null prodSet
     then Nothing
     else Just prodSet

stateNexts :: Ord a => State a -> [(State a, a)]
stateNexts st = map (\x -> (fromJust $ nextStateForElem x st, x))
                    (S.toList $ nextElemsInState st)

expandNTerm :: Ord a => Grammar a -> a -> State a
expandNTerm g nt = S.fromList $ do
  rule <- ntermRules g nt
  return $ start rule

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

stateNext :: Ord a => State a -> a -> Maybe (State a)
stateNext st nt =
  let st' = S.fromList $ mapMaybe (next nt) (S.toList st)
  in if S.null st' then Nothing else Just st'

prodStates :: Ord a => Grammar a -> [ProdState a]
prodStates g = do
  rule@(Rule _ prod) <- rules g
  i <- [0..length prod]
  return $ ProdState rule i

prodStateClosures :: Ord a => Grammar a -> [(ProdState a, State a)]
prodStateClosures g = do
  prodState <- prodStates g
  let state = expandClosure g (S.singleton prodState)
  return (prodState, state)

createLR0States :: Ord a => Grammar a -> Graph (State (FullElem a)) () (Maybe a)
createLR0States g = GR.unfold
    traverse
    GR.combineEq
    GR.combineEq
    (Node (initialState fullGrammar) ())
  where
    fullGrammar = createFullGrammar g
    traverse (Node s ()) = let
      nexts = stateNexts s

      pairToEdge (s', Elem a) = (Node s' (), a)
      pairToEdge (_, Start) = error "Unexpected start symbol in rule"

      in map pairToEdge nexts
