-- | A standard inefficient Earley parser
module Parser.Earley where

import           Data.Maybe    (mapMaybe)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           ProdState     hiding (next, nextState)
import qualified ProdState     as PS

-- Helpers
----------

listOpToSetOp :: (Ord a, Ord b) => ([a] -> [b]) -> Set a -> Set b
listOpToSetOp f = Set.fromList . f . Set.toList

setConcatMap :: (Ord a, Ord b) => (a -> Maybe b) -> Set a -> Set b
setConcatMap f = listOpToSetOp (mapMaybe f)

-- Data Structures
------------------

data ParseItem a = ParseItem
  { prodState :: ProdState a
  , origin    :: Int
  } deriving (Show, Eq, Ord)

type EarleyState a = Set (ParseItem a)

type EarleyStateSequence a = Seq (EarleyState a)

next :: Ord a => a -> ParseItem a -> Maybe (ParseItem a)
next a it = do
  nextState <- PS.next a (prodState it) 
  return it { prodState = nextState }

reduce :: Ord a => Int -> ParseItem a -> EarleyStateSequence a -> EarleyStateSequence a
reduce idx item st =
  if complete $ prodState item
  then let
    orig = origin item
    items = st `Seq.index` orig
    l = PS.lhs $ prodState item
    validNextItems = setConcatMap (next l) items
    in Seq.adjust (Set.union validNextItems) idx st
  else st

shift :: Ord a => Int -> a -> EarleyStateSequence a -> EarleyStateSequence a
shift idx = undefined

