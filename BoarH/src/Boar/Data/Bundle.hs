{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Boar.Data.Bundle
  ( Bundle()
  , fromList
  , empty
  , merge
  , cons
  , append
  , map
  , parallel
  , toLists
  ) where

import           Control.Applicative (pure, (<$>), (<*>))
import           Control.Monad       (liftM)
import           Control.Monad.Fix   (MonadFix)
import           Data.Foldable       (Foldable)
import qualified Data.Foldable       as F
import           Boar.Data.IRef
import           Prelude             hiding (foldr, map)
import qualified Prelude             as P

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

foldr :: forall s a b. (a -> b -> b)
      -> (b -> b -> b)
      -> b
      -> Bundle s a
      -> b
foldr stepF mergeF emptyF (Bundle bundle)  = structuralMap go bundle
  where
    go :: (c -> b) -> BundleBase' a c -> b
    go get b' = case b' of
      Next' a i -> stepF a (get i)
      Merge' i1 i2 -> mergeF (get i1) (get i2)
      End' -> emptyF

bFoldrM :: forall s a b m. (MonadFix m)
        => (a -> b -> m b)
        -> (b -> b -> m b)
        -> m b
        -> Bundle s a
        -> m b
bFoldrM stepF mergeF emptyF (Bundle bundle)  = structuralMapM go bundle
  where
    go :: (c -> b) -> BundleBase' a c -> m b
    go get b' = case b' of
      Next' a i -> stepF a (get i)
      Merge' i1 i2 -> mergeF (get i1) (get i2)
      End' -> emptyF

toLists :: Bundle s a -> [[a]]
toLists = foldr
  (\a -> P.map (a:))
  (++)
  [[]]

append :: Bundle s a -> Bundle s a -> RT s (Bundle s a)
append b1 b2 = bFoldrM cons merge (return b2) b1

map :: (a -> b) -> Bundle s a -> RT s (Bundle s b)
map f = bFoldrM (cons . f) merge empty

test :: RT s (Bundle s Int)
test = do
  x <- fromList [2]
  y <- cons 3 x
  z <- cons 4 x
  w <- merge y z
  cons 5 w


result :: [[Int]]
result = runRT (liftM toLists test)
