{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Boar.Util.ConstrainedFunctor where

import GHC.Exts (Constraint)
  
class Container co where
  type ContainerDatum co a :: Constraint
  empty :: co a
  null :: ContainerDatum co a => co a -> Bool
  singleton :: ContainerDatum co a => a -> co a
  append :: ContainerDatum co a => co a -> co a -> co a