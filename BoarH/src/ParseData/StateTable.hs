{-# LANGUAGE ExistentialQuantification #-}
module ParseData.StateTable where

import           Boar.Data.MultiMap (MultiMap)
import           Data.Map           (Map)
import           Grammar            (Rule)

{-|
A sequence of reductions to apply to the current token stream.
-}
type Reductions a = [Rule a]

data ResultStates k a = ResultStates
  { incState   :: k
  , freshState :: Maybe k
  , reductions :: [Reductions a]
  } deriving (Eq, Ord, Show)

{-|
For a given Earley state, this shows which nonterminals are complete in this
state, as well as what transitions are available, and their resulting
result states.
-}
data EarleyInfo k a = EarleyInfo
  { complete    :: MultiMap a (Rule a)
  , transitions :: Map a (ResultStates k a)
  } deriving (Eq, Ord, Show)

{-|

-}
data StateCollection k a = StateCollection
  { startState :: k
  , states     :: Map k (EarleyInfo k a)
  } deriving (Eq, Ord, Show)


-- Cursors, which can follow a state collection.

data Cursor a = forall k . Ord k => Cursor
  { collection :: StateCollection k a
  , currState  :: k
  }

newCursor :: Ord k => StateCollection k a -> Cursor a
newCursor col = Cursor
  { collection = col
  , currState = startState col
  }

newtype CursorInfo a = CursorInfo (EarleyInfo (Cursor a) a)
