module ProdState where

import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe (mapMaybe)
import           Data.Set   (Set)
import qualified Data.Set   as S
import           Fixpoint
import           Grammar    hiding (lhs)
import qualified Grammar    as G
import           Graph      (Graph)
import qualified Graph      as GR

-- | Represents a cursor position within a rule. Classically, this is
-- represented with a dot at the appropriate location in a rule.
--
-- For example: a -> b . c
--
data ProdState a = ProdState (Rule a) Int
  deriving (Eq, Ord)

instance Show a => Show (ProdState a) where
  show (ProdState (Rule l r) i) =
    let (pre, post) = splitAt i r
    in show l ++ " -> " ++ unwords ( map show pre ++ ["."] ++ map show post )

type State a = Set (ProdState a)

lhs :: ProdState a -> a
lhs (ProdState r _) = G.lhs r

start :: Rule a -> ProdState a
start r = ProdState r 0

complete :: ProdState a -> Bool
complete (ProdState (Rule _ prod) i) = length prod == i

atPoint :: ProdState a -> Maybe a
atPoint (ProdState (Rule _ prod) i) = index prod i
  where
    index (a:_) 0 = Just a
    index (_:r) idx | idx > 0 = index r (idx - 1)
    index _ _ = Nothing

inc :: ProdState a -> ProdState a
inc (ProdState r i) = ProdState r (i + 1)

next :: (Eq a) => ProdState a -> a -> Maybe (ProdState a)
next ps e = if atPoint ps == Just e then Just (inc ps) else Nothing

expandNTerm :: Ord a => Grammar a -> a -> State a
expandNTerm g nt = S.fromList $ do
  rule <- ntermRules g nt
  return $ start rule

expandProdState :: Ord a => Grammar a -> ProdState a -> State a
expandProdState g ps = case atPoint ps of
  Just nt | isnterm g nt -> expandNTerm g nt
  _ -> S.empty

expandClosure :: Ord a => Grammar a -> State a -> State a
expandClosure g = fixpointEq iter
  where iter set = S.unions $ set : S.toList (S.map (expandProdState g) set)
  

stateNexts :: Ord a => State a -> Set a
stateNexts st = S.fromList $ mapMaybe atPoint (S.toList st)

stateNext :: Ord a => State a -> a -> Maybe (State a)
stateNext st nt =
  let st' = S.fromList $ mapMaybe (`next` nt) (S.toList st)
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

createStates :: Ord a => Grammar a -> a -> Graph (State a) () a
createStates g = undefined

-----------

data ParseMap a = Map (State a) (Map a (State a))

concatMaybeMap :: Ord a => Map a (Maybe b) -> Map a b
concatMaybeMap = undefined

nextState :: Ord a => Grammar a -> State a -> Map a (State a)
nextState g st = M.map (expandClosure g) $ concatMaybeMap $ M.fromSet (stateNext st) (stateNexts st)

