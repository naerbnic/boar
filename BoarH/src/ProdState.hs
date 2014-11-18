module ProdState where

import           Control.Monad (liftM)
import           Data.Maybe    (isNothing)
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Grammar
import qualified Grammar       as G

-- Helpers
-----------

maybeToSet :: Maybe a -> Set a
maybeToSet Nothing = S.empty
maybeToSet (Just a) = S.singleton a

listIndexMaybe :: [a] -> Int -> Maybe a
listIndexMaybe (a:_) 0 = Just a
listIndexMaybe (_:r) i | i > 0 = listIndexMaybe r (i - 1)
listIndexMaybe _ _ = Nothing

-- | Represents a cursor position within a rule. Classically, this is
-- represented with a dot at the appropriate location in a rule.
--
-- For example: @a -> b . c@
--
data ProdState a = ProdState
  { rule :: Rule a
  , pos  :: Int
  } deriving (Eq, Ord)

instance Show a => Show (ProdState a) where
  show (ProdState (Rule l r) i) =
    let (pre, post) = splitAt i r
    in show l ++ " -> " ++ unwords ( map show pre ++ ["."] ++ map show post )

-- | For a production state @a -> b . c@, returns @a@.
lhs :: ProdState a -> a
lhs (ProdState r _) = G.lhs r

-- | For a rule @a -> b c@, returns the production state @a -> . b c@.
start :: Rule a -> ProdState a
start r = ProdState r 0

-- | Advance the point by one step in the prod state, if it is not at the
-- end, and return the 'Just' of the advanced-over element, and the advanced
-- prod state, or 'Nothing' if the point is at the end
step :: ProdState a -> Maybe (a, ProdState a)
step (ProdState r@(Rule _ prod) i) = do
  a <- listIndexMaybe prod i
  return (a, ProdState r (i + 1))

-- | Returns true if the dot in the production state is at the end.
--
-- For example: @a -> b c .@ and @a -> .@ would both return true.
complete :: ProdState a -> Bool
complete ps = isNothing (step ps)

-- | Returns the 'Just' of the element just after the dot in a production state,
-- or 'Nothing' if the production state is complete.
--
-- For example: @ a -> b . c d @ will return @'Just' c@.
atPoint :: ProdState a -> Maybe a
atPoint = liftM fst . step

-- | Returns the 'Just' of the production state obtained by incrementing the
-- dot forward one element, or 'Nothing' if the production state is complete.
inc :: ProdState a -> Maybe (ProdState a)
inc = liftM snd . step

-- | @next a ps@ returns the 'Just' of the production state obtained by
-- incrementing the dot forward one element if @a@ was just after the dot, or
-- 'Nothing' if it the element was different, or incomplete.
next :: (Eq a) => a -> ProdState a -> Maybe (ProdState a)
next e ps = if atPoint ps == Just e then inc ps else Nothing
