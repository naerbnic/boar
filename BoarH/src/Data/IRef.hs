{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RecursiveDo  #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-|
Defines a references thread monad similar to 'ST' that provides immutable references
that can be compared against each other. It however does not allow for them
to be modified. 

RT is protected by a state thread variable s, which ensures that all references
created within the monad are unique. This means that references can't escape
the monad they are created in.

This also contains an equivalent of Data.Reify's typeclasses for IRefs, using
those refs as the points of the graph.
-}
module Data.IRef
  ( RT()
  , IRef()
  , newIRef
  , readIRef
  , runRT
  , IRefKey()
  , getIRefKey
  , MuIRef(..)
  , structuralMap
  , structuralMapM
  ) where

import           Control.Applicative
import           Control.Monad              (ap)
import           Control.Monad.Fix
import           Control.Monad.ST
import           Control.Monad.State.Strict (State, evalState, get, put)
import qualified Data.Map                   as M
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.STRef
import Data.Traversable (traverse)
import Control.Monad.Identity (runIdentity)

newtype RT s a = RT { getState :: State Int a }

instance Monad (RT s) where
  return = RT . return
  m >>= f = RT $ getState m >>= getState . f

instance MonadFix (RT s) where
  mfix f = RT $ mfix (getState . f)

instance Functor (RT s) where
  fmap f a = RT $ fmap f (getState a)

instance Applicative (RT s) where
  pure = return
  (<*>) = ap

{-|
Reference to a value of type @a@. Two references which are equal will return
the exact same value referrentially when readIRef is called.
-}
data IRef s a = IRef
  { ref   :: Int
  , value :: a
  }

instance Eq (IRef s a) where
  a == b = ref a == ref b

instance Ord (IRef s a) where
  compare a b = compare (ref a) (ref b)

-- | Creates a new reference for the given value. This reference is guaranteed
-- to be unique from all other references created in this monad.
newIRef :: a -> RT s (IRef s a)
newIRef v = RT $ do
  i <- get
  put (i + 1)
  return (IRef i v)

-- | Returns the value from a reference. Since references are immutable, this
-- does not use the 'RT' monad
readIRef :: IRef s a -> a
readIRef = value

-- | Runs the given RT monad to obtain a value. The typesystem ensures that
-- @a@ does not refer to the type s.
runRT :: (forall s . RT s a) -> a
runRT m = evalState (getState m) 0

-- | An orderable opaque key which is associated with a reference.
newtype IRefKey s = IRefKey Int
  deriving (Eq, Ord)

-- | Returns a unique key value for the given reference.
getIRefKey :: IRef s a -> IRefKey s
getIRefKey = IRefKey . ref

-- | A typeclass similar to 'Data.Reify.MuRef' which allows for graph algorithms
-- to be run on IRef based data structures.
class MuIRef s a | a -> s where
  type DeIRef a :: * -> *
  mapDeIRef :: Applicative f
            => (forall b . (MuIRef s b, DeIRef a ~ DeIRef b) => IRef s b -> f u)
            -> IRef s a -> f (DeIRef a u)

data IGraph s a = IGraph [(IRefKey s, a)] (IRefKey s)

reifyRTGraph :: (MuIRef s a) => IRef s a -> IGraph s (DeIRef a (IRefKey s))
reifyRTGraph a = runST $ do
  rt1 <- newSTRef Set.empty
  rt2 <- newSTRef []
  root <- findNodes rt1 rt2 a
  pairs <- readSTRef rt2
  return (IGraph pairs root)

findNodes :: (MuIRef t a)
          => STRef s (Set (IRefKey t))
          -> STRef s [(IRefKey t, DeIRef a (IRefKey t))]
          -> IRef t a
          -> ST s (IRefKey t)
findNodes rt1 rt2 a = do
  let key = getIRefKey a
  tab <- readSTRef rt1
  if key `Set.member` tab
    then return key
    else do
      writeSTRef rt1 $ Set.insert key tab
      res <- mapDeIRef (findNodes rt1 rt2) a
      modifySTRef rt2 ((key, res):)
      return key
      
structuralMapM :: (MuIRef s t, MonadFix m, Applicative m)
               => (forall a . (a -> b) -> DeIRef t a -> m b)
               -> IRef s t -> m b
structuralMapM f t = let
  IGraph edges start = reifyRTGraph t
  m = M.fromList edges
  in do
    rec m' <- traverse id $ M.map (f (m' M.!)) m 
    return $ m' M.! start
    
structuralMap :: (MuIRef s t) 
              => (forall a . (a -> b) -> DeIRef t a -> b)
              -> IRef s t -> b
structuralMap f t = runIdentity $ structuralMapM (\innerF -> return . f innerF) t

{-

As reference to the Data.Reify version.


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
-}


