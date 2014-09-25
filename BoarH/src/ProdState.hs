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

-- | A parse state defined as a collection of production states.
type State a = Set (ProdState a)

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

nextElemsInState :: Ord a => State a -> Set a
nextElemsInState st = S.fromList $ mapMaybe atPoint $ S.toList st

nextStateForElem :: Ord a => a -> State a -> Maybe (State a)
nextStateForElem el st = let
  prodSet = S.fromList $ mapMaybe (next el) $ S.toList st
  in if S.null prodSet
     then Nothing
     else Just prodSet

stateNexts :: Ord a => State a -> [(State a, a)]
stateNexts st = map (\x -> (fromJust $ nextStateForElem x st, x))
                    (S.toList $ nextElemsInState st)

expandNTerm :: Ord a => Grammar a -> a -> State a
expandNTerm g nt = S.fromList $ do
  rule <- ntermRules g nt
  return $ start rule
  
initialState :: Ord a => Grammar a -> State a
initialState g = expandClosure g (expandNTerm g (G.start g))

expandProdStateNT :: Ord a => Grammar a -> ProdState a -> State a
expandProdStateNT g ps = case atPoint ps of
  Just nt | isnterm g nt -> expandNTerm g nt
  _ -> S.empty
  
expandProdStateNullable :: Ord a => Grammar a -> ProdState a -> State a
expandProdStateNullable g ps = maybeToSet $ do
  pt <- atPoint ps
  if pt `S.member` nullables g
    then inc ps
    else Nothing

expandClosure :: Ord a => Grammar a -> State a -> State a
expandClosure g = fixpointSet 
  (mergeSetFunctions [expandProdStateNT g, expandProdStateNullable g])

stateNext :: Ord a => State a -> a -> Maybe (State a)
stateNext st nt =
  let st' = S.fromList $ mapMaybe (next nt) (S.toList st)
  in if S.null st' then Nothing else Just st'

prodStates :: Ord a => Grammar a -> [ProdState a]
prodStates g = do
  rule@(Rule _ prod) <- rules g
  i <- [0..length prod]
  return $ ProdState rule i

prodStateClosures :: Ord a => Grammar a -> [(ProdState a, State a)]
prodStateClosures g = do
  prodState <- prodStates g
  let state = expandClosure g (S.singleton prodState)
  return (prodState, state)

createLR0States :: Ord a => Grammar a -> Graph (State (FullElem a)) () (Maybe a)
createLR0States g = GR.unfold
    traverse
    GR.combineEq
    GR.combineEq
    (Node (initialState fullGrammar) ())
  where
    fullGrammar = createFullGrammar g
    traverse (Node s ()) = let
      nexts = stateNexts s
      
      pairToEdge (s', Elem a) = (Node s' (), a)
      pairToEdge (_, Start) = error "Unexpected start symbol in rule"
      
      in map pairToEdge nexts
