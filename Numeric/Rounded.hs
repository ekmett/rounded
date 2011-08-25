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
    -- ** Random stuff (TODO: sort this out)
    , toString
    ) where

import Control.Applicative
import Data.Proxy
import Data.Ratio
import Data.Word
import Data.Reflection
import GHC.Integer.GMP.Internals
import GHC.Exts (Int(..)) 
import Foreign.C.Types
import Foreign.C.String
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

foreign import prim "mpfr_cmm_get_str" mpfrGetString#
  :: CRounding# -> Int# -> CSignPrec# -> CExp# -> ByteArray# -> (# Int#, ByteArray# #)

toString# :: ByteArray# -> String
toString# ba# = go 0# where
  go i | i <# (sizeofByteArray# ba# -# 1#) = C# (unsafeCoerce# (indexWord8Array# ba# i)) : go (i +# 1#)
       | otherwise = []

toString :: forall r p. Rounding r => Int -> Rounded r p -> String
toString (I# base) (Rounded s e l) =
  case mpfrGetString# (mode# (Proxy::Proxy r)) base s e l of
    (# d, buf #) -> let (x, y) = splitAt (I# d) (toString# buf) in x ++ "." ++ y

instance Rounding r => Show (Rounded r p) where
  showsPrec d (Rounded s e l) = showsPrec d (D# (mpfrGetDouble# (mode# (Proxy::Proxy r)) s e l))

type Binop = CSignPrec# -> CExp# -> ByteArray#
          -> CSignPrec# -> CExp# -> ByteArray#
          -> (# CSignPrec#, CExp#, ByteArray# #)

foreign import prim "mpfr_cmm_add" mpfrAdd# :: CRounding# -> Binop
foreign import prim "mpfr_cmm_sub" mpfrSub# :: CRounding# -> Binop
foreign import prim "mpfr_cmm_mul" mpfrMul# :: CRounding# -> Binop
foreign import prim "mpfr_cmm_div" mpfrDiv# :: CRounding# -> Binop

instance Eq (Rounded r p) 

instance (Rounding r, Precision p) => Num (Rounded r p) where
  Rounded s e l + Rounded s' e' l' = case mpfrAdd# (mode# (Proxy::Proxy r)) s e l s' e' l' of
    (# s'', e'', l'' #) -> Rounded s'' e'' l''
  Rounded s e l - Rounded s' e' l' = case mpfrSub# (mode# (Proxy::Proxy r)) s e l s' e' l' of
    (# s'', e'', l'' #) -> Rounded s'' e'' l''
  Rounded s e l * Rounded s' e' l' = case mpfrMul# (mode# (Proxy::Proxy r)) s e l s' e' l' of
    (# s'', e'', l'' #) -> Rounded s'' e'' l''
  fromInteger (S# i) = case mpfrFromInt# (prec# (Proxy::Proxy p)) i of
    (# s, e, l #) -> Rounded s e l
  fromInteger (J# s l) = case mpfrFromInteger# (prec# (Proxy::Proxy p)) s l of
    (# s, e, l #) -> Rounded s e l

foreign import prim "mpfr_cmm_init_si" mpfrFromInt#
  :: CPrecision# -> Int# -> (# CSignPrec#, CExp#, ByteArray# #)

foreign import prim "mpfr_cmm_init_z" mpfrFromInteger#
  :: CPrecision# -> Int# -> ByteArray# -> (# CSignPrec#, CExp#, ByteArray# #)

foreign import prim "mpfr_cmm_init_q" mpfrFromRational#
  :: CRounding# -> CPrecision#
  -> Int# -> ByteArray#
  -> Int# -> ByteArray#
  -> (# CSignPrec#, CExp#, ByteArray# #)

instance (Rounding r, Precision p) => Fractional (Rounded r p) where
  fromRational x = case (numerator x, denominator x) of
    (S# n#, S# d#) -> case int2Integer# n# of
      (# ns#, nl# #) -> case int2Integer# d# of
        (# ds#, dl# #) -> conv ns# nl# ds# dl#
    (J# ns# nl#, S# d#) -> case int2Integer# d# of
      (# ds#, dl# #) -> conv ns# nl# ds# dl#
    (S# n#, J# ds# dl#) -> case int2Integer# n# of
      (# ns#, nl# #) -> conv ns# nl# ds# dl#
    (J# ns# nl#, J# ds# dl#) -> conv ns# nl# ds# dl#
    where
    conv :: Int# -> ByteArray# -> Int# -> ByteArray# -> Rounded r p
    conv ns# nl# ds# dl# =
      case mpfrFromRational# (mode# (Proxy::Proxy r)) (prec# (Proxy::Proxy p)) ns# nl# ds# dl# of
        (# s, e, l #) -> Rounded s e l
  Rounded s e l / Rounded s' e' l' =
    case mpfrDiv# (mode# (Proxy::Proxy r)) s e l s' e' l' of
      (# s'', e'', l'' #) -> Rounded s'' e'' l''

proxyRounding :: Rounded r p -> Proxy r
proxyRounding _ = Proxy

proxyPrecision :: Rounded r p -> Proxy p
proxyPrecision _ = Proxy

fromInt :: (Rounding r, Precision p) => Int -> Rounded r p
fromInt (I# i) = r where 
  r = case mpfrFromInt# (prec# (proxyPrecision r)) i of
    (# s, e, l #) -> Rounded s e l

foreign import prim "mpfr_cmm_init_d" mpfrFromDouble#
  :: CRounding# -> CPrecision# -> Double# -> (# CSignPrec#, CExp#, ByteArray# #)

fromDouble :: (Rounding r, Precision p) => Double -> Rounded r p 
fromDouble (D# d) = r where 
  r = case mpfrFromDouble# (mode# (proxyRounding r)) (prec# (proxyPrecision r)) d of
    (# s, e, l #) -> Rounded s e l
