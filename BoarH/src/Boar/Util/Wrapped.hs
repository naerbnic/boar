{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Boar.Util.Wrapped where

import Prelude hiding (id, (.))
import Control.Category

import GHC.Exts (Constraint)

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

class S a where
  type F a :: Constraint
  
type None a = (() :: Constraint)

a :: None a => a -> a
a = id