{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
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
import qualified Data.IntMap         as IM
import           Data.Monoid
import           Data.Reify
import           Prelude
import qualified Prelude             as P
import           System.IO.Unsafe    (unsafePerformIO)

{-|
An efficient functional data structure for keeping parallel lists
-}
data Bundle a
  = Next a (Bundle a)
  | Merge (Bundle a) (Bundle a)
  | End

fromList :: [a] -> Bundle a
fromList = P.foldr Next End

empty :: Bundle a
empty = End

-- @merge b1 b2@ does the equivalent of @
merge :: Bundle a -> Bundle a -> Bundle a
merge = Merge

cons :: a -> Bundle a -> Bundle a
cons = Next

-- | Adds all elements in the foldable as separate lists to Bundle. Must have
-- at least one element
parallel :: Foldable t => t a -> Bundle a
parallel fld = P.foldr1 Merge $ P.map (`Next` End) $ F.toList fld

data Bundle' a ref
  = Next' a ref
  | Merge' ref ref
  | End'
  deriving Show

instance MuRef (Bundle a) where
  type DeRef (Bundle a) = Bundle' a

  mapDeRef f b = case b of
    Next a b' -> Next' a <$> f b'
    Merge b1 b2 -> Merge' <$> f b1 <*> f b2
    End -> pure End'

structuralMapIO :: MuRef t => (forall a . (a -> b) -> DeRef t a -> b) -> t -> IO b
structuralMapIO f t = do
  Graph edges start <- reifyGraph t
  return $ let
    im = IM.fromList edges
    im' = IM.map (f (im' IM.!)) im
    in im' IM.! start

{-# NOINLINE structuralMap #-}
structuralMap :: MuRef t => (forall a . (a -> b) -> DeRef t a -> b) -> t -> b
structuralMap f t =
  -- Although generally unsafe, performing the reification and conversion on
  -- MuRefable data structures should have no side effects overall, and there is
  -- no chance of a leak, so this should be safe.
  unsafePerformIO $ structuralMapIO f t

toLists :: Bundle a -> [[a]]
toLists = structuralMap innerToLists
  where
    innerToLists f bv = case bv of
      Next' a i -> P.map (a:) (f i)
      Merge' i1 i2 -> f i1 ++ f i2
      End' -> [[]]

append :: forall a . Bundle a -> Bundle a -> Bundle a
append b1 b2 = structuralMap go b1
  where
    go :: (b -> Bundle a) -> Bundle' a b -> Bundle a
    go f b = case b of
      Next' a i -> Next a (f i)
      Merge' i1 i2 -> Merge (f i1) (f i2)
      End' -> b2

map :: forall a b . (a -> b) -> Bundle a -> Bundle b
map f b = structuralMap go b
  where
    go :: (c -> Bundle b) -> Bundle' a c -> Bundle b
    go get b = case b of
      Next' a i -> Next (f a) (get i)
      Merge' i1 i2 -> Merge (get i1) (get i2)
      End' -> End

instance Monoid (Bundle a) where
  mempty = End
  mappend = append

test :: Bundle Int
test = let
  x = fromList [2]
  y = Next 3 x
  z = Next 4 x
  w = Merge y z
  r = Next 5 w
  in r
