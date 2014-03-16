{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Rounded.Rounding
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  LPGPL
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Various rounding modes
----------------------------------------------------------------------------
module Numeric.Rounded.Rounding
  ( Rounding(..)
  , RoundingMode(..)
  , reifyRounding
  ) where

import Data.Data
import Data.Proxy
import Data.Singletons
import GHC.Prim

data RoundingMode
  = TowardNearestWithTiesAwayFromZero
  | TowardNearest
  | TowardZero
  | TowardInf
  | TowardNegInf
  | AwayFromZero
  | Faithfully
  deriving (Eq,Ord,Show,Read,Data,Typeable)

class Rounding (r :: RoundingMode) where mode# :: p r -> Int#
instance Rounding TowardNearest where mode# _ = 0#
instance Rounding TowardZero    where mode# _ = 1#
instance Rounding TowardInf     where mode# _ = 2#
instance Rounding TowardNegInf  where mode# _ = 3#
instance Rounding AwayFromZero  where mode# _ = 4#
instance Rounding Faithfully    where mode# _ = 5#
instance Rounding TowardNearestWithTiesAwayFromZero where mode# _ = -1#

instance Enum RoundingMode where
  toEnum (-1) = TowardNearestWithTiesAwayFromZero
  toEnum 0 = TowardNearest
  toEnum 1 = TowardZero
  toEnum 2 = TowardInf
  toEnum 3 = TowardNegInf
  toEnum 4 = AwayFromZero
  toEnum 5 = Faithfully
  toEnum _ = error "out of range"

  fromEnum TowardNearestWithTiesAwayFromZero = -1
  fromEnum TowardNearest = 0
  fromEnum TowardZero = 1
  fromEnum TowardInf = 2
  fromEnum TowardNegInf = 3
  fromEnum AwayFromZero = 4
  fromEnum Faithfully = 5

instance Bounded RoundingMode where
  minBound = TowardNearestWithTiesAwayFromZero
  maxBound = Faithfully

newtype instance Sing (m :: RoundingMode) = SRounding RoundingMode

instance SingKind ('KProxy :: KProxy RoundingMode) where
  type DemoteRep ('KProxy :: KProxy RoundingMode) = RoundingMode
  fromSing (SRounding n) = n
  toSing n = SomeSing (SRounding n)

instance SingI TowardNearestWithTiesAwayFromZero where sing = SRounding TowardNearestWithTiesAwayFromZero
instance SingI TowardNearest where sing = SRounding TowardNearest
instance SingI TowardZero    where sing = SRounding TowardZero
instance SingI TowardInf     where sing = SRounding TowardInf
instance SingI TowardNegInf  where sing = SRounding TowardNegInf
instance SingI AwayFromZero  where sing = SRounding AwayFromZero
instance SingI Faithfully    where sing = SRounding Faithfully

reifyRounding :: RoundingMode -> (forall s. Rounding s => Proxy s -> r) -> r
reifyRounding TowardNearestWithTiesAwayFromZero f = f (Proxy :: Proxy TowardNearestWithTiesAwayFromZero)
reifyRounding TowardNearest                     f = f (Proxy :: Proxy TowardNearest)
reifyRounding TowardZero                        f = f (Proxy :: Proxy TowardZero)
reifyRounding TowardInf                         f = f (Proxy :: Proxy TowardInf)
reifyRounding TowardNegInf                      f = f (Proxy :: Proxy TowardNegInf)
reifyRounding AwayFromZero                      f = f (Proxy :: Proxy AwayFromZero)
reifyRounding Faithfully                        f = f (Proxy :: Proxy Faithfully)
{-# INLINE reifyRounding #-}
