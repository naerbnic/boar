{-# LANGUAGE ScopedTypeVariables #-}
module Boar.Util.Memo where

import qualified Data.HashTable.IO as HT
import System.Mem.StableName
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace

safeMemo :: forall a b . (a -> b) -> IO (a -> IO b)
safeMemo f = do
  memoTable :: HT.CuckooHashTable (StableName a) b <- trace "Created!" HT.new
  return $ \x -> do
    sn <- x `seq` makeStableName x
    value <- HT.lookup memoTable sn
    case value of
      Just b -> return b
      Nothing -> do
        let result = trace "Reify!" $ f x
        HT.insert memoTable sn result
        return result
        
data MemoFn a b = MemoFn !(a -> b)

memo :: (a -> b) -> a -> b
memo f = let
  f' = (unsafePerformIO $! safeMemo f)
  in (unsafePerformIO . f')
  
test :: [[Int]]
test = let
  x = [1, 2, 3]
  y = [x, x]
  g = memo reverse
  in map g y

{-# NOINLINE getStableName #-}
getStableName :: a -> StableName a
getStableName a = a `seq` unsafePerformIO (makeStableName a)
  
stableNameTest :: Bool
stableNameTest = let
  x :: [Int] = [1, 2, 3]
  y = getStableName x
  z = getStableName x
  in y == z