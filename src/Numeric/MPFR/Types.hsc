#include <ghc-gmp.h>
#include <mpfr.h>

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.MPFR.Types
-- Copyright   :  (C) 2015-2017 Claude Heiland-Allen
-- License     :  LGPL
-- Stability   :  experimental
-- Portability :  non-portable
--
-- MPFR types.
----------------------------------------------------------------------------
module Numeric.MPFR.Types where

import Data.Data
import Data.Typeable
import Data.Bits
import Data.Ix
import Data.Int
import Data.Word

import Foreign (Storable(..), Ptr)

import Numeric.GMP.Types (MPLimb(..))

-- | @mpfr_t@
data MPFR = MPFR
  { mpfrPrec :: !MPFRPrec
  , mpfrSign :: !MPFRSign
  , mpfrExp :: !MPFRExp
  , mpfrD :: !(Ptr MPLimb)
  }

instance Storable MPFR where
  sizeOf _ = (#size __mpfr_struct)
  alignment _ = (#alignment __mpfr_struct)
  peek ptr = do
    prec <- (#peek __mpfr_struct, _mpfr_prec) ptr
    sign <- (#peek __mpfr_struct, _mpfr_sign) ptr
    exp' <- (#peek __mpfr_struct, _mpfr_exp) ptr
    d <- (#peek __mpfr_struct, _mpfr_d) ptr
    return (MPFR{ mpfrPrec = prec, mpfrSign = sign, mpfrExp = exp', mpfrD = d })
  poke ptr (MPFR{ mpfrPrec = prec, mpfrSign = sign, mpfrExp = exp', mpfrD = d }) = do
    (#poke __mpfr_struct, _mpfr_prec) ptr prec
    (#poke __mpfr_struct, _mpfr_sign) ptr sign
    (#poke __mpfr_struct, _mpfr_exp) ptr exp'
    (#poke __mpfr_struct, _mpfr_d) ptr d

-- | @mpfr_int@
newtype MPFRInt = MPFRInt (#type mpfr_int)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @mpfr_uint@
newtype MPFRUInt = MPFRUInt (#type mpfr_uint)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @mpfr_long@
newtype MPFRLong = MPFRLong (#type mpfr_long)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @mpfr_ulong@
newtype MPFRULong = MPFRULong (#type mpfr_ulong)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @mpfr_size_t@
newtype MPFRSize = MPFRSize (#type mpfr_size_t)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @mpfr_prec_t@
newtype MPFRPrec = MPFRPrec (#type mpfr_prec_t)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @mpfr_uprec_t@
newtype MPFRUPrec = MPFRUPrec (#type mpfr_uprec_t)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @mpfr_sign_t@
newtype MPFRSign = MPFRSign (#type mpfr_sign_t)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @mpfr_exp_t@
newtype MPFRExp = MPFRExp (#type mpfr_exp_t)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @mpfr_uexp_t@
newtype MPFRUExp = MPFRUExp (#type mpfr_uexp_t)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @mpfr_rnd_t@
newtype MPFRRnd = MPFRRnd (#type mpfr_rnd_t)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @mpfr_kind_t@
newtype MPFRKind = MPFRKind (#type mpfr_kind_t)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)
