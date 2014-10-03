{-# LANGUAGE TypeFamilies, TupleSections, RankNTypes #-}
module Data.Bundle
  ( Bundle()
  , fromList
  , singleton
  , empty
  , merge
  , cons
  , toLists
  ) where

import Data.Reify
import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.IntMap as IM
import System.IO.Unsafe (unsafePerformIO)

{-|
An efficient functional data structure for keeping parallel lists
-}
data Bundle a
  = Next a (Bundle a)
  | Merge (Bundle a) (Bundle a)
  | End

fromList :: [a] -> Bundle a
fromList = foldr Next End

-- @singleton lst@ returns a bundle that is equivalent to @[lst]@.
singleton :: [a] -> Bundle a
singleton = foldr Next End

empty :: Bundle a
empty = End

-- @merge b1 b2@ does the equivalent of @
merge :: Bundle a -> Bundle a -> Bundle a
merge = Merge

cons :: a -> Bundle a -> Bundle a
cons = Next

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
structuralMap f t = unsafePerformIO $ structuralMapIO f t

toLists :: Bundle a -> [[a]]
toLists = structuralMap innerToLists
  where
    innerToLists f bv = case bv of
      Next' a i -> map (a:) (f i)
      Merge' i1 i2 -> f i1 ++ f i2
      End' -> [[]]
      
test :: Bundle Int
test = let
  x = fromList [2]
  y = Next 3 x
  z = Next 4 x
  w = Merge y z
  r = Next 5 w
  in r