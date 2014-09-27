module ProdState where

import           Data.Maybe (mapMaybe, fromJust)
import           Data.Set   (Set)
import qualified Data.Set   as S
import           Fixpoint
import           Grammar    hiding (lhs, start)
import qualified Grammar    as G
import           Graph      (Graph, Node(..))
import qualified Graph      as GR

-- Helpers
-----------

maybeToSet :: Maybe a -> Set a
maybeToSet Nothing = S.empty
maybeToSet (Just a) = S.singleton a

-- | Represents a cursor position within a rule. Classically, this is
-- represented with a dot at the appropriate location in a rule.
--
-- For example: @a -> b . c@
--
data ProdState a = ProdState (Rule a) Int
  deriving (Eq, Ord)

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

-- | Returns true if the dot in the production state is at the end.
--
-- For example: @a -> b c .@ and @a -> .@ would both return true.
complete :: ProdState a -> Bool
complete (ProdState (Rule _ prod) i) = length prod == i

-- | Returns the 'Just' of the element just after the dot in a production state,
-- or 'Nothing' if the production state is complete.
--
-- For example: @ a -> b . c d @ will return @'Just' c@.
atPoint :: ProdState a -> Maybe a
atPoint (ProdState (Rule _ prod) i) = index prod i
  where
    index (a:_) 0 = Just a
    index (_:r) idx | idx > 0 = index r (idx - 1)
    index _ _ = Nothing

-- | Returns the 'Just' of the production state obtained by incrementing the
-- dot forward one element, or 'Nothing' if the production state is complete.
inc :: ProdState a -> Maybe (ProdState a)
inc ps@(ProdState r i) = if complete ps
  then Nothing
  else Just $ ProdState r (i + 1)

-- | @next a ps@ returns the 'Just' of the production state obtained by
-- incrementing the dot forward one element if @a@ was just after the dot, or
-- 'Nothing' if it the element was different, or incomplete. 
next :: (Eq a) => a -> ProdState a -> Maybe (ProdState a)
next e ps = if atPoint ps == Just e then inc ps else Nothing
