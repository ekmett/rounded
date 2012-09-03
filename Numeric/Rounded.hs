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
    -- * Useful Constants
    , kLog2
    , kEuler
    , kCatalan
    -- ** Random stuff (TODO: sort this out)
    , toString
    ) where

import Data.Proxy
import Data.Bits
import GHC.Integer.GMP.Internals
import GHC.Integer.GMP.Prim
import GHC.Prim
import GHC.Types
import GHC.Real
import GHC.Int
import Numeric.Rounded.Precision
import Control.Parallel()
import Numeric.Rounded.Rounding
import System.IO.Unsafe()

type CSignPrec#  = Int#
type CPrecision# = Int#
type CExp#       = Int#
type CRounding#  = Int#

prec_bit :: Int
prec_bit | b63 == 0 = b31
         | otherwise = b63
  where b63 = bit 63
        b31 = bit 31

data Rounded r p = Rounded
  { roundedSignPrec :: CSignPrec# -- Sign# * Precision#
  , roundedExp      :: CExp#
  , roundedLimbs    :: ByteArray#
  }

-- We could use this in a rewrite rule for fast conversions to Double...
-- foreign import prim "mpfr_cmm_get_d"       mpfr_cmm_get_d :: CRounding# -> CSignPrec# -> CExp# -> ByteArray# -> Double#

foreign import prim "mpfr_cmm_get_str"     mpfr_cmm_get_str :: CRounding# -> Int# -> CSignPrec# -> CExp# -> ByteArray# -> (# Int#, ByteArray# #)

toString# :: ByteArray# -> String
toString# ba# = go 0# where
  go i | i <# (sizeofByteArray# ba# -# 1#) = C# (unsafeCoerce# (indexWord8Array# ba# i)) : go (i +# 1#)
       | otherwise = []

-- This whole thing is a bit of a hack attempting to replicate the hack that is formatRealFloat, but it should do for now
toString :: forall r p. Rounding r => Int -> Rounded r p -> String
toString (I# base) (Rounded s e l) = 
  -- FIXME: Less hackish would be to use isNaN or infinity instead of breaking up the string...
  case buf of
    '-':'@':_ -> "-Infinity"
    '-':ds    -> '-' : withExponent ex ds
    '@':'I':_ -> "Infinity"
    '@':'N':_ -> "NaN"
    ds        -> withExponent ex ds
  where
  withExponent _ [] = ""
  withExponent (-1) ds = "0." ++ ds
  withExponent q dds@(d:ds) | q < -1 = d : '.' : ds ++ "e" ++ show q
                            | q > 6  = d : '.' : ds ++ "e" ++ show (q - 1)
                            | otherwise = let (x, y) = splitAt q dds in x ++ "." ++ y

  (ex, buf) = case mpfr_cmm_get_str (mode# (Proxy::Proxy r)) base s e l of
    (# e#, buf# #) -> (I# e#, toString# buf#)

instance Rounding r => Show (Rounded r p) where
  showsPrec _ = showString . toString 10 -- showsPrec d (D# (mpfr_cmm_get_d (mode# (Proxy::Proxy r)) s e l))

-- N.B.: similar to Unary, assumes that output precision is same as precsion of _second_ operand
type Binary
  = CRounding# ->
    CSignPrec# -> CExp# -> ByteArray# ->
    CSignPrec# -> CExp# -> ByteArray# ->
    (# CSignPrec#, CExp#, ByteArray# #)

foreign import prim "mpfr_cmm_add" mpfrAdd# :: Binary
foreign import prim "mpfr_cmm_sub" mpfrSub# :: Binary
foreign import prim "mpfr_cmm_mul" mpfrMul# :: Binary
foreign import prim "mpfr_cmm_div" mpfrDiv# :: Binary
foreign import prim "mpfr_cmm_min" mpfrMin# :: Binary
foreign import prim "mpfr_cmm_max" mpfrMax# :: Binary

type Comparison
  = CSignPrec# -> CExp# -> ByteArray# ->
    CSignPrec# -> CExp# -> ByteArray# ->
    Int#

foreign import prim "mpfr_cmm_equal_p"         mpfrEqual#    :: Comparison
foreign import prim "mpfr_cmm_lessgreater_p"   mpfrNotEqual# :: Comparison
foreign import prim "mpfr_cmm_less_p"          mpfrLess# :: Comparison
foreign import prim "mpfr_cmm_greater_p"       mpfrGreater# :: Comparison
foreign import prim "mpfr_cmm_lessequal_p"     mpfrLessEqual# :: Comparison
foreign import prim "mpfr_cmm_greaterequal_p"  mpfrGreaterEqual# :: Comparison

cmp :: (CSignPrec# -> CExp# -> ByteArray# -> CSignPrec# -> CExp# -> ByteArray# -> Int#) -> Rounded r p -> Rounded r p -> Bool
cmp f (Rounded s e l) (Rounded s' e' l') = I# (f s e l s' e' l') /= 0

binary :: Rounding r => Binary -> Rounded r p -> Rounded r p -> Rounded r p
binary f (Rounded s e l) (Rounded s' e' l') = r where
  r = case f (mode# (proxyRounding r)) s e l s' e' l' of
    (# s'', e'', l'' #) -> Rounded s'' e'' l''


instance Eq (Rounded r p) where
  (==) = cmp mpfrEqual#
  (/=) = cmp mpfrNotEqual#

foreign import prim "mpfr_cmm_cmp" mpfrCmp# :: CSignPrec# -> CExp# -> ByteArray# -> CSignPrec# -> CExp# -> ByteArray# -> Int#

instance Rounding r => Ord (Rounded r p) where
  compare (Rounded s e l) (Rounded s' e' l') = compare (fromIntegral (I# (mpfrCmp# s e l s' e' l'))) (0 :: Int32)
  (<=) = cmp mpfrLessEqual#
  (>=) = cmp mpfrGreaterEqual#
  (<) = cmp mpfrLess#
  (>) = cmp mpfrGreater#
  min = binary mpfrMin#
  max = binary mpfrMax#

foreign import prim "mpfr_cmm_sgn" mpfrSgn# :: CSignPrec# -> CExp# -> ByteArray# -> Int#

instance (Rounding r, Precision p) => Num (Rounded r p) where
  (+) = binary mpfrAdd#
  (-) = binary mpfrSub#
  (*) = binary mpfrMul#
  fromInteger (S# i) = case mpfrFromInt# (prec# (Proxy::Proxy p)) i of
    (# s, e, l #) -> Rounded s e l
  fromInteger (J# i xs) = case mpfrFromInteger# (prec# (Proxy::Proxy p)) i xs of
    (# s, e, l #) -> Rounded s e l
  abs (Rounded s e l) = case I# s .&. complement prec_bit of
    I# s' -> Rounded s' e l
  signum (Rounded s e l) = case compare (fromIntegral sgn) (0 :: Int32) of
    LT -> -1
    EQ -> 0
    GT -> 1
    where sgn = I# (mpfrSgn# s e l)

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
  fromRational x = case x of
    S# n# :% S# d# -> case int2Integer# n# of
      (# ns#, nl# #) -> case int2Integer# d# of
        (# ds#, dl# #) -> conv ns# nl# ds# dl#
    J# ns# nl# :% S# d# -> case int2Integer# d# of
      (# ds#, dl# #) -> conv ns# nl# ds# dl#
    S# n# :% J# ds# dl# -> case int2Integer# n# of
      (# ns#, nl# #) -> conv ns# nl# ds# dl#
    J# ns# nl# :% J# ds# dl# -> conv ns# nl# ds# dl#
    where
    conv :: Int# -> ByteArray# -> Int# -> ByteArray# -> Rounded r p
    conv ns# nl# ds# dl# =
      case mpfrFromRational# (mode# (Proxy::Proxy r)) (prec# (Proxy::Proxy p)) ns# nl# ds# dl# of
        (# s, e, l #) -> Rounded s e l

  (/) = binary mpfrDiv#

proxyRounding :: Rounded r p -> Proxy r
proxyRounding _ = Proxy

proxyPrecision :: Rounded r p -> Proxy p
proxyPrecision _ = Proxy

-- TODO: shouldn't this take a rounding, too? It's conceivable that an Int might exceed the requested
-- precision, which would require rounding.
fromInt :: Precision p => Int -> Rounded r p
fromInt (I# i) = r where
  r = case mpfrFromInt# (prec# (proxyPrecision r)) i of
    (# s, e, l #) -> Rounded s e l

foreign import prim "mpfr_cmm_init_d" mfpr_cmm_init_d
  :: CRounding# -> CPrecision# -> Double# -> (# CSignPrec#, CExp#, ByteArray# #)

fromDouble :: (Rounding r, Precision p) => Double -> Rounded r p
fromDouble (D# d) = r where
  r = case mfpr_cmm_init_d (mode# (proxyRounding r)) (prec# (proxyPrecision r)) d of
    (# s, e, l #) -> Rounded s e l

-- N.B.: This (and the corresponding CMM) assumes that you want same precision as the
-- operand. Is this what we want? All the standard Haskell typeclasses are homogeneous
-- in the type, and since the precision is recorded in the type, this seems like a safe
-- assumption, but perhaps someone might want to ask for a higher precision for output?
type Unary
  = CRounding# ->
    CSignPrec# -> CExp# -> ByteArray# ->
    (# CSignPrec#, CExp#, ByteArray# #)

foreign import prim "mpfr_cmm_log" mpfrLog# :: Unary
foreign import prim "mpfr_cmm_exp" mpfrExp# :: Unary
foreign import prim "mpfr_cmm_sqrt" mpfrSqrt# :: Unary
foreign import prim "mpfr_cmm_sin" mpfrSin# :: Unary
foreign import prim "mpfr_cmm_cos" mpfrCos# :: Unary
foreign import prim "mpfr_cmm_tan" mpfrTan# :: Unary
foreign import prim "mpfr_cmm_asin" mpfrArcSin# :: Unary
foreign import prim "mpfr_cmm_acos" mpfrArcCos# :: Unary
foreign import prim "mpfr_cmm_atan" mpfrArcTan# :: Unary
foreign import prim "mpfr_cmm_sinh" mpfrSinh# :: Unary
foreign import prim "mpfr_cmm_cosh" mpfrCosh# :: Unary
foreign import prim "mpfr_cmm_tanh" mpfrTanh# :: Unary
foreign import prim "mpfr_cmm_asinh" mpfrArcSinh# :: Unary
foreign import prim "mpfr_cmm_acosh" mpfrArcCosh# :: Unary
foreign import prim "mpfr_cmm_atanh" mpfrArcTanh# :: Unary

unary :: Rounding r => Unary -> Rounded r p -> Rounded r p
unary f (Rounded s e l) = r where
  r = case f (mode# (proxyRounding r)) s e l of
    (# s', e', l' #) -> Rounded s' e' l'


type Constant = CRounding# -> CPrecision# -> (# CSignPrec#, CExp#, ByteArray# #)

foreign import prim "mpfr_cmm_const_pi"      mpfrConstPi#      :: Constant

constant :: (Rounding r, Precision p) => Constant -> Rounded r p
constant k = r where
  r = case k (mode# (proxyRounding r)) (prec# (proxyPrecision r)) of
      (# s, e, l #) -> Rounded s e l

instance (Rounding r, Precision p) => Floating (Rounded r p) where
  pi = constant mpfrConstPi#
  exp = unary mpfrExp#
  sqrt = unary mpfrSqrt#
  log = unary mpfrLog#
  sin = unary mpfrSin#
  tan = unary mpfrTan#
  cos = unary mpfrCos#
  asin = unary mpfrArcSin#
  atan = unary mpfrArcTan#
  acos = unary mpfrArcCos#
  sinh = unary mpfrSinh#
  tanh = unary mpfrTanh#
  cosh = unary mpfrCosh#
  asinh = unary mpfrArcSinh#
  atanh = unary mpfrArcTanh#
  acosh = unary mpfrArcCosh#


foreign import prim "mpfr_cmm_const_log2"    mpfrConstLog2# :: Constant
foreign import prim "mpfr_cmm_const_euler"   mpfrConstEuler# :: Constant
foreign import prim "mpfr_cmm_const_catalan" mpfrConstCatalan# :: Constant

-- | Natural logarithm of 2
kLog2 :: (Rounding r, Precision p) => Rounded r p
kLog2 = constant mpfrConstLog2#
-- | 0.577...
kEuler :: (Rounding r, Precision p) => Rounded r p
kEuler = constant mpfrConstEuler#
-- | 0.915...
kCatalan :: (Rounding r, Precision p) => Rounded r p
kCatalan = constant mpfrConstCatalan#

--foreign import prim "mpfr_cmm_atan2" mpfrArcTan2# :: Binary
--instance (Rounding r, Precision p) => RealFloat (Rounded r p) where
--  atan2 = binary mpfrArcTan2#