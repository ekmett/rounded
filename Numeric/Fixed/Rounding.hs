{-# LANGUAGE Rank2Types, MagicHash #-}
module Numeric.Fixed.Rounding 
  ( Round(..)
  , Rounding(..)
  , Near, Zero, Up, Down
  , near, zero, up, down
  , reifyRounding
  ) where

import GHC.Prim

data Rounding
  = Down 
  | Up 
  | Near 
  | Zero

data Round a = Round Int#

-- todo: fix these constants

data Down
down :: Round Down
down = Round 0#

data Up
up :: Round Up
up = Round 1#

data Zero 
zero :: Round Zero
zero = Round 2#

data Near
near :: Round Near
near = Round 3#

reifyRounding :: Rounding -> (forall r. Round r -> a) -> a
reifyRounding Down f = f down
reifyRounding Up   f = f up 
reifyRounding Near f = f near
reifyRounding Zero f = f zero
{-# INLINE reifyRounding #-}
