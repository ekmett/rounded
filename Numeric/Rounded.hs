{-# LANGUAGE ForeignFunctionInterface
           , GHCForeignImportPrim
           , MagicHash
           , UnboxedTuples
           , UnliftedFFITypes 
           , ScopedTypeVariables
           , Rank2Types
           , TemplateHaskell
           , CPP
           #-}

module Numeric.Rounded
    ( 
    -- * floating point numbers with a specified rounding mode and precision
      Rounded(..)
    , fromInt
    , fromDouble
    -- * Precision
    , Precision(precision)
    , bits 		-- create a precision with a given number of bits at compile time
    , bytes 		-- create a precision with a given number of bytes at compile time
    , reifyPrecision 	-- create a precision with a given number of bits at runtime
    -- * Rounding 
    , Rounding
    -- ** Rounding Modes
    , TowardNearest
    , TowardZero
    , TowardInf
    , TowardNegInf
    , AwayFromZero
    , Faithfully
    , TowardNearestWithTiesAwayFromZero
    ) where

import Control.Applicative
import Data.Proxy
import Data.Ratio
import Data.Word
import Data.Reflection
import GHC.Integer.GMP.Internals
import GHC.Exts (Int(..)) 
import Foreign.C.Types
import Numeric.Precision
import Numeric.Rounding
import GHC.Prim
import GHC.Types
import GHC.Word
import GHC.Int

type CSign#      = Int#
type CSignPrec#  = Int#
type CPrecision# = Int#
type CExp#       = Int#
type CRounding#  = Int#

data Rounded r p = Rounded 
  { roundedSignPrec :: CSignPrec# -- Sign# * Precision#
  , roundedExp      :: CExp#
  , roundedLimbs    :: ByteArray#
  }

foreign import prim "mpfr_cmm_get_d" mpfrGetDouble#
  :: CRounding# -> CSignPrec# -> CExp# -> ByteArray# -> Double# 

instance Rounding r => Show (Rounded r p) where
  showsPrec d (Rounded s e l) = showsPrec d (D# (mpfrGetDouble# (mode# (Proxy::Proxy r)) s e l))
  
foreign import prim "mpfr_cmm_add" mpfrAdd#
  :: CRounding#
  -> CSignPrec# -> CExp# -> ByteArray#
  -> CSignPrec# -> CExp# -> ByteArray#
  -> (# CSignPrec#, CExp#, ByteArray# #)

instance Eq (Rounded r p) 

instance (Rounding r, Precision p) => Num (Rounded r p) where
  m@(Rounded s e l) + Rounded s' e' l' = case mpfrAdd# (mode# (Proxy::Proxy r)) s e l s' e' l' of
    (# s'', e'', l'' #) -> Rounded s'' e'' l''
  fromInteger (S# i) = case mpfrFromInt# (prec# (Proxy::Proxy p)) i of
    (# s, e, l #) -> Rounded s e l

foreign import prim "mpfr_cmm_init_si" mpfrFromInt#
  :: CPrecision# -> Int# -> (# CSignPrec#, CExp#, ByteArray# #)

proxyRounding :: Rounded r p -> Proxy r
proxyRounding _ = Proxy

proxyPrecision :: Rounded r p -> Proxy p
proxyPrecision _ = Proxy

fromInt :: Precision p => Int -> Rounded r p
fromInt (I# i) = r where 
  r = case mpfrFromInt# (prec# (proxyPrecision r)) i of
    (# s, e, l #) -> Rounded s e l

foreign import prim "mpfr_cmm_init_d" mpfrFromDouble#
  :: CRounding# -> CPrecision# -> Double# -> (# CSignPrec#, CExp#, ByteArray# #)

fromDouble :: (Rounding r, Precision p) => Double -> Rounded r p 
fromDouble (D# d) = r where 
  r = case mpfrFromDouble# (mode# (proxyRounding r)) (prec# (proxyPrecision r)) d of
    (# s, e, l #) -> Rounded s e l
