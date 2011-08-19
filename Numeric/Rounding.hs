{-# LANGUAGE MagicHash, EmptyDataDecls #-}
module Numeric.Rounding
  ( Rounding(..)
  , TowardNearest
  , TowardZero
  , TowardInf
  , TowardNegInf
  , AwayFromZero
  , Faithfully
  , TowardNearestWithTiesAwayFromZero
  ) where

import GHC.Prim

class Rounding r where
  mode# :: proxy r -> Int#

data TowardNearest 
instance Rounding TowardNearest where
  mode# _ = 0#

data TowardZero 
instance Rounding TowardZero where
  mode# _ = 1# 

data TowardInf
instance Rounding TowardInf where
  mode# _ = 2#

data TowardNegInf
instance Rounding TowardNegInf where
  mode# _ = 3#

data AwayFromZero
instance Rounding AwayFromZero where
  mode# _ = 4#

data Faithfully
instance Rounding Faithfully where
  mode# _ = 5#
 
data TowardNearestWithTiesAwayFromZero
instance Rounding TowardNearestWithTiesAwayFromZero where
  mode# _ = -1#
