module ParseData.Action
  ( Action(..)
  , ActionSequence
  ) where

import Boar.Base.Rule (Rule)

data Action a
  = Reduce (Rule a)
  | Shift a
  
type ActionSequence a = [Action a]