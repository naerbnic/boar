{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Boar.Util.Lens where

import Prelude hiding (id, (.))
import Control.Category
import Data.Set (Set)
import qualified Data.Set as Set
  
data Lens a b = Lens
  { get :: a -> b
  , set :: a -> b -> a
  }
  
instance Category Lens where
  id = wrapper id id
  l1 . l2 = Lens
    { get = get l1 . get l2
    , set = \a b -> set l2 a $ set l1 (get l2 a) b
    }
  
modify :: Lens a b -> (b -> b) -> a -> a
modify l f a = set l a $ f $ get l a

wrapper :: (a -> b) -> (b -> a) -> Lens a b
wrapper to from = Lens
  { get = to
  , set = const from
  }

setList :: Ord a => Lens (Set a) [a]
setList = wrapper Set.toList Set.fromList
