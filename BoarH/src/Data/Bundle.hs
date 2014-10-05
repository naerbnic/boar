{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
module Data.Bundle
  ( Bundle()
  , fromList
  , empty
  , merge
  , cons
  , append
  , parallel
  , toLists
  ) where

import           Control.Applicative (pure, (<$>), (<*>))
import           Data.Foldable       (Foldable)
import qualified Data.Foldable       as F
import           Prelude
import qualified Prelude             as P
import           Data.IRef
import Control.Monad (liftM)

-- Helpers
----------

foldrM :: Monad m => (a -> b -> m b) -> [a] -> m b -> m b
foldrM f lst initial = case lst of
  [] -> initial
  a:r -> foldrM f r initial >>= f a
  
foldl1M :: Monad m => (a -> a -> m a) -> [a] -> m a
foldl1M _ [a] = return a
foldl1M f (a:r) = foldl1M f r >>= f a
foldl1M _ _ = error "Data.Bundle.fold1M"

{-|
An efficient functional data structure for keeping parallel lists
-}
data BundleBase s a
  = Next a (Bundle s a)
  | Merge (Bundle s a) (Bundle s a)
  | End
  
newtype Bundle s a = Bundle (IRef s (BundleBase s a))

liftBase :: BundleBase s a -> RT s (Bundle s a)
liftBase b = liftM Bundle $ newIRef b

fromList :: [a] -> RT s (Bundle s a)
fromList lst = foldrM cons lst empty

empty :: RT s (Bundle s a)
empty = liftBase End

-- @merge b1 b2@ does the equivalent of @
merge :: Bundle s a -> Bundle s a -> RT s (Bundle s a)
merge b1 b2 = liftBase (Merge b1 b2)

cons :: a -> Bundle s a -> RT s (Bundle s a)
cons a b = liftBase (Next a b)

-- | Adds all elements in the foldable as separate lists to Bundle. Must have
-- at least one element
parallel :: Foldable t => t a -> RT s (Bundle s a)
parallel fld = do
  e <- empty
  s <- mapM (`cons` e) (F.toList fld)
  foldl1M merge s

data BundleBase' a ref
  = Next' a ref
  | Merge' ref ref
  | End'
  deriving Show

instance MuIRef s (BundleBase s a) where
  type DeIRef (BundleBase s a) = BundleBase' a

  mapDeIRef f b = case readIRef b of
    Next a (Bundle b') -> Next' a <$> f b'
    Merge (Bundle b1) (Bundle b2) -> Merge' <$> f b1 <*> f b2
    End -> pure End'

toLists :: Bundle s a -> [[a]]
toLists (Bundle b) = structuralMap innerToLists b
  where
    innerToLists f bv = case bv of
      Next' a i -> P.map (a:) (f i)
      Merge' i1 i2 -> f i1 ++ f i2
      End' -> [[]]

append :: forall s a . Bundle s a -> Bundle s a -> RT s (Bundle s a)
append (Bundle b1) b2 = structuralMapM go b1
  where
    go :: (b -> Bundle s a) -> BundleBase' a b -> RT s (Bundle s a)
    go f b = case b of
      Next' a i -> cons a (f i)
      Merge' i1 i2 -> merge (f i1) (f i2)
      End' -> return b2

map :: forall s a b . (a -> b) -> Bundle s a -> RT s (Bundle s b)
map f (Bundle b) = structuralMapM go b
  where
    go :: (c -> Bundle s b) -> BundleBase' a c -> RT s (Bundle s b)
    go get b' = case b' of
      Next' a i -> cons (f a) (get i)
      Merge' i1 i2 -> merge (get i1) (get i2)
      End' -> empty

test :: RT s (Bundle s Int)
test = do
  x <- fromList [2]
  y <- cons 3 x
  z <- cons 4 x
  w <- merge y z
  cons 5 w
  
  
result :: [[Int]]
result = runRT (liftM toLists test)
