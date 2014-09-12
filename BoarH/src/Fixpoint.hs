module Fixpoint where

fixpointEq :: Eq a => (a -> a) -> a -> a
fixpointEq f = go
  where
    go a = let 
        a' = f a
      in if a == a' then a else go a'