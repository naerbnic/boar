-- | A standard inefficient Earley parser
module Parser.Earley where

import           Data.Maybe    (mapMaybe, fromJust)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           MultiMap      (MultiMap)
import qualified MultiMap      as MM
import           ProdState     hiding (next, step)
import qualified ProdState     as PS
import           Prelude       hiding (init)
import           Grammar
import           Data.Bundle   (Bundle)
import qualified Data.Bundle   as B

import           Generate.EarleyStates hiding (stateTransitions)
import qualified Generate.EarleyStates as ES

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

-- Data Structures
------------------

data Inst a = Reduce (Rule a)
            | Shift a
            
type ParseResult a = Bundle (Inst a)

data Item k = Item
  { itemState :: k
  , itemOrigin :: Int
  } deriving (Eq, Ord)

data EarleyState k a = EarleyState
  { stateItems :: Map (Item k) (ParseResult a)
  , stateTransitions :: Map (Maybe a) (Map (Item k) (Bundle (Rule a)))
  } 

type EarleyStateSequence k a = Seq (EarleyState k a)

-- Item Functions
-----------------

reduceItem :: (Ord k, Ord a)
           => StateCollection k a
           -> EarleyStateSequence k a
           -> Item k
           -> Map (Item k) (Bundle (Rule a))
reduceItem col stateSeq (Item k i) = let
  completeTransitions = MM.toMap $ ES.complete (states col Map.! k)
  
  reduceNT a ruleSet = let
    ruleBundle = B.parallel ruleSet
    originState = stateSeq `Seq.index` i
    transition = stateTransitions originState Map.! Just a
    in Map.map (`B.append` ruleBundle) transition
    
  mapList = map (uncurry reduceNT) $ Map.toList completeTransitions
  
  in Map.unionsWith B.merge mapList

-- Main Parsing Functions
-------------------------

init :: Ord k => StateCollection k (Maybe a) -> EarleyState k a
init = undefined

step :: Ord k => StateCollection k (Maybe a) -> EarleyStateSequence k a -> a -> EarleyState k a
step = undefined

end :: (Ord k, Ord a) => StateCollection k (Maybe a) -> EarleyStateSequence k a -> ParseResult a
end col st = let
  lastState = seqLast st
  
  lastItems = stateItems lastState

  result = Map.lookup Nothing (stateTransitions lastState)
  in undefined

run :: (Ord k, Ord a) => StateCollection k (Maybe a) -> [a] -> ParseResult a
run col lst = let
  initSeq = Seq.singleton (init col)
  
  stepSeq currSeq a = let
    nextState = step col currSeq a
    in currSeq Seq.|> nextState
  
  result = foldl stepSeq initSeq lst
  in end col result