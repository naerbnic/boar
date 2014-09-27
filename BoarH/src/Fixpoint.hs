module Fixpoint where

import           Data.Set (Set)
import qualified Data.Set as S

fixpointEq :: Eq a => (a -> a) -> a -> a
fixpointEq f = go
  where
    go a = let
        a' = f a
      in if a == a' then a else go a'

{-|
@fixpointSet f s@ performs a fixpoint operation on the initial set @s@. @f@ is
called on each member of @s@, generating potentially values that aren't in @s@.
@f@ is called transitively on these until no new values are generated.

The function @f@ will be applied to each element of the final set @s'@. By
definition, applying @f@ to each element of @s'@ will generate a set of values
which is a subset of @s'@.
-}
fixpointSet :: Ord a => (a -> Set a) -> Set a -> Set a
fixpointSet f s = let
  go seen work = if S.null work
    then seen
    else let
      nextSet = S.unions $ map f (S.toList work)
      in go (seen `S.union` nextSet) (nextSet `S.difference` seen)
  in go s s

mergeSetFunctions :: Ord a => [a -> Set a] -> a -> Set a
mergeSetFunctions fs a = S.unions (map ($ a) fs)
