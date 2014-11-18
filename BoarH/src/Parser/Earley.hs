-- | A standard inefficient Earley parser
module Parser.Earley where

import           Boar.Data.Bundle           (Bundle)
import qualified Boar.Data.Bundle           as B
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (fromJust, mapMaybe)
import           Data.Sequence         (Seq)
import qualified Data.Sequence         as Seq
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Grammar
import qualified Boar.Data.MultiMap              as MM
import           Prelude               hiding (init)

import           Generate.EarleyStates hiding (stateTransitions)
import qualified Generate.EarleyStates as ES

import           Control.Monad         (foldM)
import           Boar.Data.IRef
import           Data.Traversable      (traverse)

-- Helpers
----------

listOpToSetOp :: (Ord a, Ord b) => ([a] -> [b]) -> Set a -> Set b
listOpToSetOp f = Set.fromList . f . Set.toList

setConcatMap :: (Ord a, Ord b) => (a -> Maybe b) -> Set a -> Set b
setConcatMap f = listOpToSetOp (mapMaybe f)

seqLastMaybe :: Seq a -> Maybe a
seqLastMaybe s = case Seq.viewr s of
  _ Seq.:> a -> Just a
  Seq.EmptyR -> Nothing

seqLast :: Seq a -> a
seqLast = fromJust . seqLastMaybe

partialLift2 :: Monad m => (a -> b -> m r) -> m a -> m b -> m r
partialLift2 f ma mb = do
  a <- ma
  b <- mb
  f a b

-- Data Structures
------------------

data Inst a = Reduce (Rule a)
            | Shift a

type ParseResult s a = Bundle s (Inst a)

data Item k = Item
  { itemState  :: k
  , itemOrigin :: Int
  } deriving (Eq, Ord)

data EarleyState s k a = EarleyState
  { stateItems       :: Map (Item k) (ParseResult s a)
  , stateTransitions :: Map (Maybe a) (Map (Item k) (Bundle s (Rule a)))
  }

type EarleyStateSequence s k a = Seq (EarleyState s k a)

-- Item Functions
-----------------

reduceItem :: (Ord k, Ord a)
           => StateCollection k a
           -> EarleyStateSequence s k a
           -> Item k
           -> RT s (Map (Item k) (Bundle s (Rule a)))
reduceItem col stateSeq (Item k i) = let
  completeTransitions = MM.toMap $ ES.complete (states col Map.! k)

  reduceNT a ruleSet = do
    ruleBundle <- B.parallel ruleSet
    let originState = stateSeq `Seq.index` i
    let transition = stateTransitions originState Map.! Just a
    return $ Map.map (`B.append` ruleBundle) transition

  in do
    mapList <- mapM (uncurry reduceNT) (Map.toList completeTransitions)
    traverse id $ Map.unionsWith (partialLift2 B.merge) mapList

-- Main Parsing Functions
-------------------------

init :: Ord k
     => StateCollection k (Maybe a)
     -> RT s (EarleyState s k a)
init = undefined

step :: Ord k
     => StateCollection k (Maybe a)
     -> EarleyStateSequence s k a
     -> a
     -> RT s (EarleyState s k a)
step = undefined

end :: (Ord k, Ord a)
    => StateCollection k (Maybe a)
    -> EarleyStateSequence s k a
    -> RT s (ParseResult s a)
end col st = let
  lastState = seqLast st

  lastItems = stateItems lastState

  result = Map.lookup Nothing (stateTransitions lastState)
  in undefined

run :: (Ord k, Ord a) => StateCollection k (Maybe a) -> [a] -> RT s (ParseResult s a)
run col lst = do
  initState <- init col
  let initSeq = Seq.singleton initState

  result <- let
    stepSeq currSeq a = do
      nextState <- step col currSeq a
      return $ currSeq Seq.|> nextState

    in foldM stepSeq initSeq lst

  end col result
