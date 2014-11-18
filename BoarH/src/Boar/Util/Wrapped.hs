{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Boar.Util.Wrapped where

import Prelude hiding (id, (.))
import Control.Category
import Data.Set (Set)
import qualified Data.Set as Set

data Wrapping a b = Wrapping
  { wrap :: a -> b
  , unwrap :: b -> a
  }
  
instance Category Wrapping where
  id = Wrapping id id
  w1 . w2 = Wrapping
    { wrap = wrap w1 . wrap w2
    , unwrap = unwrap w2 . unwrap w1
    }
  
asWrapped :: Wrapping a b -> (b -> b) -> a -> a
asWrapped w f = unwrap w . f . wrap w

setList :: Ord a => Wrapping (Set a) [a]
setList = Wrapping
  { wrap = Set.toList
  , unwrap = Set.fromList
  }
