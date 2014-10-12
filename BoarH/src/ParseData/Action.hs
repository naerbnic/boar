module ParseData.Action
  ( Action(..)
  , ActionSequence
  ) where

import Grammar (Rule)

data Action a
  = Reduce (Rule a)
  | Shift a
  
type ActionSequence a = [Action a]