{-# LANGUAGE   ForeignFunctionInterface,  GHCForeignImportPrim,
  MagicHash,
  UnboxedTuples,
  UnliftedFFITypes #-}

module Numeric.Fixed.Prim 
  ( CPrecision#
  , CSign#
  , CExp#
  , CRounding#
  , fixed2Double#
  , double2Fixed#
--, fixedAdd#
  , test
  , idd
  , ByteArray(..)
  ) where

import GHC.Prim
import GHC.Types
import GHC.Word
import GHC.Int

type CPrecision# = Word#
type CSign# = Int#
type CExp# = Int#
type CRounding# = Word#

foreign import prim "fixed_cmm_double2Fixedzh" double2Fixed#
  :: CRounding# -> CPrecision# -> Double# -> (# CPrecision#, CSign#, CExp#, ByteArray# #)

foreign import prim "fixed_cmm_fixed2Doublezh" fixed2Double#
  :: CRounding# -> CPrecision# -> CSign# -> CExp# -> ByteArray# -> Double# 

{-
foreign import prim "fixed_cmm_fixedAddzh" fixedAdd#
  :: CRounding#
  -> CPrecision# -> CSign# -> CExp# -> ByteArray#
  -> CPrecision# -> CSign# -> CExp# -> ByteArray#
  -> (# CPrecision#, CSign#, CExp#, ByteArray# #)
-}

data ByteArray = ByteArray ByteArray#

test :: (Word, Int, Int, Int) 
test = case double2Fixed# 0## 64## 0.0## of (# x, y, z, w #) -> (W# x, I# y, I# z, I# (indexIntArray# w 0#))


idd :: Word -> Word -> Double -> Double
idd (W# rnd) (W# prec) (D# d) = case double2Fixed# rnd prec d of
    (# x,y,z,w #) -> D# (fixed2Double# rnd x y z w)
