{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK not-home #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Rounded.Internal
-- Copyright   :  (C) 2012-2014 Edward Kmett, Daniel Peebles
--                (C) 2013-2019 Claude Heiland-Allen
-- License     :  BSD3
-- Maintainer  :  Claude Heiland-Allen <claude@mathr.co.uk>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Numeric.Rounded.Internal where

import Control.Exception (bracket, bracket_, throwIO, ArithException(Overflow))
import Data.Bits (shiftL, testBit)
import Data.Coerce (coerce)
import Data.Int (Int32)
import Data.Proxy (Proxy(..))
import Data.Ratio ((%))

import Foreign (with, alloca, allocaBytes, peek, sizeOf, nullPtr)
import Foreign.C (CInt(..), CIntMax(..))
import Foreign.C.String (peekCString)
import Numeric.LongDouble (LongDouble)

import System.IO.Unsafe (unsafePerformIO)

import GHC.Prim
  ( ByteArray#
  , sizeofByteArray#
  , copyByteArrayToAddr#
  , newByteArray#
  , copyAddrToByteArray#
  , unsafeFreezeByteArray#
  )
import GHC.Types (IO(..))
import GHC.Exts (Ptr(..), Int(..))

#if MIN_VERSION_base(4,9,0)
import Numeric (Floating(..))
#endif
import Numeric (readSigned, readFloat)

import Numeric.GMP.Utils (withInInteger, withOutInteger, withOutInteger_, withInRational)
import Numeric.GMP.Types (MPLimb)

import Numeric.MPFR.Types
import Numeric.MPFR.Raw

import Numeric.Rounded.Precision
import Numeric.Rounded.Rounding

type role Rounded phantom nominal

-- | A properly rounded floating-point number with a given rounding mode and precision.
--
-- You can 'Data.Coerce.coerce' to change rounding modes, but not precision.
data Rounded (r :: RoundingMode) p = Rounded
  { roundedPrec  :: !MPFRPrec
  , roundedSign  :: !MPFRSign
  , roundedExp   :: !MPFRExp
  , roundedLimbs :: !ByteArray#
  }

-- | Round to 'Double' with the given rounding mode.
toDouble :: (Rounding r, Precision p) => Rounded r p -> Double
toDouble x = unsafePerformIO $ in_ x $ \xfr -> mpfr_get_d xfr (rnd x)
-- this syntax is strange, but it seems to be the way it works...
{-# RULES "realToFrac/toDouble" forall (x :: (Rounding r, Precision p) => Rounded r p) . realToFrac x = toDouble x #-}

-- | Round to 'LongDouble' with the given rounding mode.
toLongDouble :: (Rounding r, Precision p) => Rounded r p -> LongDouble
toLongDouble x = unsafePerformIO $ in_ x $ \xfr -> with 0 $ \yfr -> with 0 $ \ffr -> wrapped_mpfr_get_ld yfr xfr (rnd x) ffr >> peek yfr
-- this syntax is strange, but it seems to be the way it works...
{-# RULES "realToFrac/toLongDouble" forall (x :: (Rounding r, Precision p) => Rounded r p) . realToFrac x = toLongDouble x #-}

-- | Round to a different precision with the given rounding mode.
precRound :: (Rounding r, Precision p1, Precision p2) => Rounded r p1 -> Rounded r p2
precRound x = unsafePerformIO $ do
  (Just y, _) <- in_ x $ \xfr -> out_ $ \yfr ->
    mpfr_set yfr xfr (rnd x)
  return y
-- TODO figure out correct syntax (if even possible) to allow RULE
-- {-# RULES "realToFrac/precRound" realToFrac = precRound #-}

toString :: (Rounding r, Precision p) => Rounded r p -> String
-- FIXME: what do about unsightly 0.1 -> 0.1000...0002 or 9.999...9995e-2 issues
toString x = unsafePerformIO $ do
  (s, e) <- in_ x $ \xfr -> with 0 $ \eptr -> do
    s <- bracket (mpfr_get_str nullPtr eptr 10 0 xfr (fromIntegral (fromEnum TowardNearest))) mpfr_free_str peekCString
    e <- peek eptr
    return (s, fromIntegral e)
  return $ case () of
    _ | isNaN x -> "NaN"
      | isInfinite x && sgn' == GT -> "Infinity"
      | isInfinite x -> "-Infinity"
      | isNegativeZero x -> "-0.0"
      | sgn' == EQ -> "0.0"
      | e <  0 ||
        e >= threshold -> sign ++ take 1 digits  ++ "." ++
                          dropTrailingZeroes (take (n - 1) (drop 1 digits0)) ++
                          "e" ++ show (e - 1)
      | e == 0         -> sign ++ "0." ++
                          dropTrailingZeroes digits
      | e <  threshold -> sign ++ take e digits0 ++ "." ++
                          dropTrailingZeroes (take (n - e) (drop e digits0))
      where
        sgn' = sgn x
        sign = case sgn' of
          GT -> ""
          EQ -> ""
          LT -> "-"
        threshold = 8
        n = length digits
        digits = case take 1 s of
          "-" -> drop 1 s
          _ -> s
        digits0 = digits ++ repeat '0'
        dropTrailingZeroes a = case dropWhile ('0' ==) (reverse a) of
          "" -> "0"
          b -> reverse b

instance (Rounding r, Precision p) => Show (Rounded r p) where
  showsPrec p x = showParen (p >= 7 && take 1 s == "-") (s ++) -- FIXME: precedence issues?
    where s = toString x

instance (Rounding r, Precision p) => Read (Rounded r p) where
  -- apparently this handles parens without any extra fuss
  readsPrec _ = readSigned readFloat -- FIXME: precedence issues?

unary
  :: (Rounding r, Precision p1, Precision p2)
  => Unary -> Rounded r p1 -> Rounded r p2
unary f a = unsafePerformIO $ do
  (Just c, _) <- in_ a $ \afr ->
    out_ $ \cfr ->
      f cfr afr (rnd a)
  return c

unary' :: Rounding r => Unary -> Rounded r p -> Rounded r p
unary' f a = unsafePerformIO $ do
  (Just c, _) <- in_ a $ \afr ->
    out_' (roundedPrec a) $ \cfr ->
      f cfr afr (rnd a)
  return c

unary'' :: Unary -> Rounded r p -> Rounded r p
unary'' f a = unsafePerformIO $ do
  (Just c, _) <- in_ a $ \afr ->
    out_' (roundedPrec a) $ \cfr ->
      f cfr afr (fromIntegral (fromEnum TowardNearest))
  return c

abs' :: Rounded r p -> Rounded r p
abs' = unary'' mpfr_abs

negate' :: Rounded r p -> Rounded r p
negate' = unary'' mpfr_neg

(.-.), (.+.), (.*.) :: Rounding r => Rounded r p -> Rounded r p -> Rounded r p
(.-.) = binary' mpfr_sub
(.+.) = binary' mpfr_add
(.*.) = binary' mpfr_mul

infixl 6 .+., .-.
infixl 7 .*.

abs_, acos_, acosh_, ai_, asin_, asinh_, atan_, atanh_, cbrt_, cos_, cosh_, cot_, coth_, csc_, csch_, digamma_, eint_, erf_, erfc_, exp_, exp10_, exp2_, expm1_, frac_, gamma_, j0_, j1_, li2_, lngamma_, log_, log10_, log1p_, log2_, neg_, rec_sqrt_, rint_, rint_ceil_, rint_floor_, rint_round_, rint_roundeven_, rint_trunc_, sec_, sech_, set_, sin_, sinh_, sqr_, sqrt_, tan_, tanh_, y0_, y1_, zeta_,
 negate_
  :: (Rounding r, Precision p1, Precision p2)
  => Rounded r p1 -> Rounded r p2
abs_ = unary mpfr_abs
acos_ = unary mpfr_acos
acosh_ = unary mpfr_acosh
ai_ = unary mpfr_ai
asin_ = unary mpfr_asin
asinh_ = unary mpfr_asinh
atan_ = unary mpfr_atan
atanh_ = unary mpfr_atanh
cbrt_ = unary mpfr_cbrt
cos_ = unary mpfr_cos
cosh_ = unary mpfr_cosh
cot_ = unary mpfr_cot
coth_ = unary mpfr_coth
csc_ = unary mpfr_csc
csch_ = unary mpfr_csch
digamma_ = unary mpfr_digamma
eint_ = unary mpfr_eint
erf_ = unary mpfr_erf
erfc_ = unary mpfr_erfc
exp_ = unary mpfr_exp
exp10_ = unary mpfr_exp10
exp2_ = unary mpfr_exp2
expm1_ = unary mpfr_expm1
frac_ = unary mpfr_frac
gamma_ = unary mpfr_gamma
j0_ = unary mpfr_j0
j1_ = unary mpfr_j1
li2_ = unary mpfr_li2
lngamma_ = unary mpfr_lngamma
log_ = unary mpfr_log
log10_ = unary mpfr_log10
log1p_ = unary mpfr_log1p
log2_ = unary mpfr_log2
neg_ = unary mpfr_neg
rec_sqrt_ = unary mpfr_rec_sqrt
rint_ = unary mpfr_rint
rint_ceil_ = unary mpfr_rint_ceil
rint_floor_ = unary mpfr_rint_floor
rint_round_ = unary mpfr_rint_round
rint_roundeven_ = unary mpfr_rint_roundeven
rint_trunc_ = unary mpfr_rint_trunc
sec_ = unary mpfr_sec
sech_ = unary mpfr_sech
set_ = unary mpfr_set
sin_ = unary mpfr_sin
sinh_ = unary mpfr_sinh
sqr_ = unary mpfr_sqr
sqrt_ = unary mpfr_sqrt
tan_ = unary mpfr_tan
tanh_ = unary mpfr_tanh
y0_ = unary mpfr_y0
y1_ = unary mpfr_y1
zeta_ = unary mpfr_zeta
negate_ = neg_

binary
  :: (Rounding r, Precision p1, Precision p2, Precision p3)
  => Binary -> Rounded r p1 -> Rounded r p2 -> Rounded r p3
binary f a b = unsafePerformIO $ do
  (Just c, _) <- in_ a $ \afr ->
    in_ b $ \bfr ->
      out_ $ \cfr ->
        f cfr afr bfr (rnd a)
  return c

add_, agm_, atan2_, copysign_, dim_, div_, fmod_, hypot_, max_, min_, mul_, pow_, sub_,
 (!+!), (!-!), (!*!), (!/!), (!**!)
  :: (Rounding r, Precision p1, Precision p2, Precision p3)
  => Rounded r p1 -> Rounded r p2 -> Rounded r p3
add_ = binary mpfr_add
agm_ = binary mpfr_agm
atan2_ = binary mpfr_atan2
copysign_ = binary mpfr_copysign
dim_ = binary mpfr_dim
div_ = binary mpfr_div
fmod_ = binary mpfr_fmod
hypot_ = binary mpfr_hypot
max_ = binary mpfr_max
min_ = binary mpfr_min
mul_ = binary mpfr_mul
pow_ = binary mpfr_pow
sub_ = binary mpfr_sub
(!+!) = add_
(!-!) = sub_
(!*!) = mul_
(!/!) = div_
(!**!) = pow_

infixl 6 !+!, !-!, `add_`, `sub_`
infixl 7 !*!, !/!, `mul_`, `div_`
infixr 8 !**!, `pow_`

binary' :: Rounding r => Binary -> Rounded r p -> Rounded r p -> Rounded r p
binary' f a b = unsafePerformIO $ do
  (Just c, _) <- in_ a $ \afr ->
    in_ b $ \bfr ->
      out_' (roundedPrec a) $ \cfr ->
        f cfr afr bfr (rnd a)
  return c

cmp' :: Comparison -> Rounded r p1 -> Rounded r p2 -> CInt
cmp' f a b = unsafePerformIO $
  in_ a $ \afr ->
  in_ b $ \bfr -> do
  f afr bfr

cmp :: Comparison -> Rounded r p1 -> Rounded r p2 -> Bool
cmp f a b = cmp' f a b /= 0

(!==!), (!/=!), (!<=!), (!>=!), (!<!), (!>!), (!<>!)
  :: (Precision p1, Precision p2)
  => Rounded r p1 -> Rounded r p2 -> Bool
(!==!) = cmp mpfr_equal_p
(!/=!) = cmp mpfr_lessgreater_p
(!<=!) = cmp mpfr_lessequal_p
(!>=!) = cmp mpfr_greaterequal_p
(!<!) = cmp mpfr_less_p
(!>!) = cmp mpfr_greater_p
(!<>!) = cmp mpfr_unordered_p

infix 4 !==!, !/=!, !<=!, !>=!, !<!, !>!, !<>!

compare_ :: (Precision p1, Precision p2) => Rounded r p1 -> Rounded r p2 -> Ordering
compare_ a b = compare (cmp' mpfr_cmp a b) 0

instance Eq (Rounded r p) where
  (==) = cmp mpfr_equal_p
  (/=) = cmp mpfr_lessgreater_p

instance Rounding r => Ord (Rounded r p) where
  compare a b = compare (cmp' mpfr_cmp a b) 0
  (<=) = cmp mpfr_lessequal_p
  (>=) = cmp mpfr_greaterequal_p
  (<) = cmp mpfr_less_p
  (>) = cmp mpfr_greater_p
  min = binary' mpfr_min
  max = binary' mpfr_max

sgn :: (Rounding r, Precision p) => Rounded r p -> Ordering
sgn x = compare (unsafePerformIO $ in_ x mpfr_sgn) 0

instance (Rounding r, Precision p) => Num (Rounded r p) where
  (+) = (.+.)
  (-) = (.-.)
  (*) = (.*.)
  negate = negate'
  fromInteger j = r where
    r = unsafePerformIO $ do
          if toInteger (minBound :: CIntMax) <= j && j <= toInteger (maxBound :: CIntMax)
          then do
            (Just x, _) <- out_ $ \jfr -> mpfr_set_sj jfr (fromInteger j :: CIntMax) (rnd r)
            return x
          else do
            (Just x, _) <- withInInteger j $ \jz -> out_ $ \jfr -> mpfr_set_z jfr jz (rnd r)
            return x
  abs = abs'
  signum x = case sgn x of
    LT -> -1
    EQ -> 0
    GT -> 1

instance (Rounding r, Precision p) => Fractional (Rounded r p) where
  fromRational q = r where -- TODO small integer optimisation
    r = unsafePerformIO $ do
          (Just x, _) <- withInRational q $ \qq -> out_ $ \qfr -> mpfr_set_q qfr qq (rnd r)
          return x
  (/) = (!/!)

-- | Construct a properly rounded floating point number from an 'Int'.
fromInt :: (Rounding r, Precision p) => Int -> Rounded r p
fromInt i = r
  where
    r = unsafePerformIO $ do
      (Just x, _) <- out_ $ \xfr -> mpfr_set_sj xfr (fromIntegral i) (rnd r)
      return x
-- TODO figure out correct syntax (if even possible) to allow RULE
-- {-# RULES "fromIntegral/fromInt" fromIntegral = fromInt #-}

-- | Construct a rounded floating point number directly from a 'Double'.
fromDouble :: (Rounding r, Precision p) => Double -> Rounded r p
fromDouble d = r
  where
    r = unsafePerformIO $ do
      (Just x, _) <- out_ $ \xfr -> mpfr_set_d xfr d (rnd r)
      return x
-- TODO figure out correct syntax (if even possible) to allow RULE
-- {-# RULES "realToFrac/fromDouble" realToFrac = fromDouble #-}

-- | Construct a rounded floating point number directly from a 'LongDouble'.
fromLongDouble :: (Rounding r, Precision p) => LongDouble -> Rounded r p
fromLongDouble d = r
  where
    r = unsafePerformIO $ do
      (Just x, _) <- out_ $ \xfr -> with d $ \dp -> with 0 $ \fp -> wrapped_mpfr_set_ld xfr dp (rnd r) fp
      return x
-- TODO figure out correct syntax (if even possible) to allow RULE
-- {-# RULES "realToFrac/fromLongDouble" realToFrac = fromLongDouble #-}


inplace :: (Ptr MPFR -> IO ()) -> Rounded r p -> Rounded r p
inplace f y = unsafePerformIO $ do
  (Just x, _) <- out_' (roundedPrec y) $ \xfr -> in_ y $ \yfr -> do
    _ <- mpfr_set xfr yfr (fromIntegral (fromEnum TowardNearest))
    f xfr
  return x

succUlp, predUlp :: Rounded r p -> Rounded r p
succUlp = inplace mpfr_nextabove
predUlp = inplace mpfr_nextbelow

constant :: (Rounding r, Precision p) => Constant -> Rounded r p
constant k = r where
  r = unsafePerformIO $ do
    (Just x, _) <- out_ $ \xfr -> k xfr (rnd r)
    return x


instance (Rounding r, Precision p) => Floating (Rounded r p) where
  pi    = kPi
  exp   = exp_
  sqrt  = sqrt_
  log   = log_
  (**)  = pow_
  sin   = sin_
  tan   = tan_
  cos   = cos_
  asin  = asin_
  atan  = atan_
  acos  = acos_
  sinh  = sinh_
  tanh  = tanh_
  cosh  = cosh_
  asinh = asinh_
  atanh = atanh_
  acosh = acosh_
#if MIN_VERSION_base(4,9,0)
  log1p = log1p_
  expm1 = expm1_
#endif

toRational' :: Precision p => Rounded r p -> Rational
toRational' r
   | e > 0     = fromIntegral (s `shiftL` e)
   | otherwise = s % (1 `shiftL` negate e)
   where (s, e) = decodeFloat' r

instance (Rounding r, Precision p) => Real (Rounded r p) where
  toRational = toRational'

modf, sin_cos, sinh_cosh :: (Rounding r, Precision p) => Rounded r p -> (Rounded r p, Rounded r p)
modf = dualOutput mpfr_modf
sin_cos = dualOutput mpfr_sin_cos
sinh_cosh = dualOutput mpfr_sinh_cosh

dualOutput :: (Rounding r, Precision p) => Binary -> Rounded r p -> (Rounded r p, Rounded r p)
dualOutput f x = unsafePerformIO $ do
  (Just y, (Just z, _)) <- in_ x $ \xfr ->
    out_ $ \yfr ->
      out_ $ \zfr ->
        f yfr zfr xfr (rnd x)
  return (y, z)

-- | Round to 'Integer' using the specified rounding mode.  Throws 'Overflow' if
--   the result cannot be represented (for example, infinities or NaN).
toInteger' :: (Rounding r, Precision p) => Rounded r p -> Integer
toInteger' x = unsafePerformIO $
  withOutInteger_ $ \yz ->
    in_ x $ \xfr ->
      with 0 $ \flagsptr -> do
        e <- wrapped_mpfr_get_z yz xfr (rnd x) flagsptr
        flags <- peek flagsptr
        case testBit flags erangeBit of
          False -> return e
          True -> throwIO Overflow

instance (Rounding r, Precision p) => RealFrac (Rounded r p) where
  properFraction r = (fromInteger (toInteger' i), f) where (i, f) = modf r
  truncate = roundFunc truncate_
  round    = roundFunc round_
  ceiling  = roundFunc ceiling_
  floor    = roundFunc floor_

roundFunc :: (Integral i, Precision p) => (Rounded TowardNearest p -> Rounded TowardNearest p) -> Rounded r p -> i
roundFunc f = fromInteger . toInteger' . f . coerce

unary_ :: (Precision p1, Precision p2) => (Ptr MPFR -> Ptr MPFR -> IO CInt) -> Rounded r p1 -> Rounded r p2
unary_ f x = unsafePerformIO $ do
  Just y <- withInRounded x $ \xp -> withOutRounded_ $ \yp -> f yp xp
  return y

truncate_, ceiling_, floor_, round_ :: (Precision p1, Precision p2) => Rounded r p1 -> Rounded r p2
truncate_ = unary_ mpfr_trunc
ceiling_  = unary_ mpfr_ceil
floor_    = unary_ mpfr_floor
round_    = unary_ (\yp xp -> mpfr_rint yp xp (fromIntegral (fromEnum TowardNearest)))

tst :: (Precision p) => Test -> Rounded r p -> Bool
tst f x = unsafePerformIO $ in_ x $ \xfr -> do
  t <- f xfr
  return (t /= 0)

decodeFloat' :: Rounded r p -> (Integer, Int)
decodeFloat' x = case (unsafePerformIO $ do
  in_ x $ \xfr -> withOutInteger $ \xz -> with 0 $ \flagsptr -> do
    e <- wrapped_mpfr_get_z_2exp xz xfr flagsptr
    flags <- peek flagsptr
    case testBit flags erangeBit of
      False -> return (fromIntegral e)
      True -> throwIO Overflow) of
  (0, _) -> (0, 0) -- mpfr_get_z_2exp returns emin instead of 0 for exponent
  me -> me

encodeFloat' :: (Rounding r, Precision p) => Integer -> Int -> Rounded r p
encodeFloat' j e = r where
  r = unsafePerformIO $ do
        (Just x, _) <- withInInteger j $ \jz -> out_ $ \xfr -> mpfr_set_z_2exp xfr jz (fromIntegral e) (rnd r)
        return x

instance (Rounding r, Precision p) => RealFloat (Rounded r p) where
  floatRadix  _ = 2
  floatDigits = self where
    self _ = p
    p = precision (0 `asTypeIn` self)
    asTypeIn :: a -> (a -> b) -> a
    asTypeIn = const

  -- FIXME: this should do for now, but the real ones can change...
  -- FIXME: do these need to be offset to match Haskell conventions?
  floatRange _ = (MPFR_EMIN_DEFAULT, MPFR_EMAX_DEFAULT)

  decodeFloat = decodeFloat'
  encodeFloat = encodeFloat'
  isNaN = tst mpfr_nan_p
  isInfinite = tst mpfr_inf_p
  isDenormalized _ = False
  isNegativeZero r = tst mpfr_zero_p r && tst mpfr_signbit r
  isIEEE _ = True -- is this a lie? it mostly behaves like an IEEE float, despite being much bigger
  atan2 = atan2_

kPi :: (Rounding r, Precision p) => Rounded r p
kPi = constant mpfr_const_pi

-- | Natural logarithm of 2
kLog2 :: (Rounding r, Precision p) => Rounded r p
kLog2 = constant mpfr_const_log2

-- | 0.577...
kEuler :: (Rounding r, Precision p) => Rounded r p
kEuler = constant mpfr_const_euler

-- | 0.915...
kCatalan :: (Rounding r, Precision p) => Rounded r p
kCatalan = constant mpfr_const_catalan


in_' :: Rounded r p -> (MPFR -> IO a) -> IO a
in_' (Rounded p s e l) f = withByteArray l $ \ptr _bytes -> f MPFR
  { mpfrPrec = p
  , mpfrSign = s
  , mpfrExp = e
  , mpfrD = ptr
  }

in_ :: Rounded r p -> (Ptr MPFR -> IO a) -> IO a
in_ x f = in_' x $ \y -> with y f


out_' :: MPFRPrec -> (Ptr MPFR -> IO a) -> IO (Maybe (Rounded r p), a)
out_' p f = allocaBytes (precBytes p) $ \d -> with
  MPFR{ mpfrPrec = p, mpfrSign = 0, mpfrExp = 0, mpfrD = d } $ \ptr -> do
  a <- f ptr
  MPFR{ mpfrPrec = p', mpfrSign = s', mpfrExp = e', mpfrD = d' } <- peek ptr
  if p /= p' then return (Nothing, a) else
    asByteArray d' (precBytes p') $ \l' -> return (Just (Rounded p' s' e' l'), a)

out_ :: Precision p => (Ptr MPFR -> IO a) -> IO (Maybe (Rounded r p), a)
out_ f = r where
  r = out_' prec f
  prec = fromIntegral (precision (t r))
  t :: IO (Maybe t, a) -> t
  t _ = undefined


-- | Use a value as a /constant/ @mpfr_t@ (attempts to modify it may explode,
--   changing the precision will explode).
withInRounded :: Rounded r p -> (Ptr MPFR -> IO a) -> IO a
withInRounded = in_

-- | Allocates and initializes a new @mpfr_t@, if the precision matches after
--   the action then it is peeked and returned.  Otherwise you get 'Nothing'.
withOutRounded :: Precision p => (Ptr MPFR -> IO a) -> IO (Maybe (Rounded r p), a)
withOutRounded f = r where
  r = alloca $ \ptr -> bracket_ (mpfr_init2 ptr prec) (mpfr_clear ptr) $ do
    a <- f ptr
    MPFR{ mpfrPrec = prec', mpfrSign = s, mpfrExp = e, mpfrD = d } <- peek ptr
    if prec /= prec'
      then return (Nothing, a)
      else asByteArray d (precBytes prec) $ \l ->
        return (Just (Rounded prec s e l), a)
  prec = fromIntegral (precision (t r))
  t :: IO (Maybe b, a) -> b
  t _ = undefined

-- | Allocates and initializes a new @mpfr_t@, if the precision matches after
--   the action then it is peeked and returned.  Otherwise you get 'Nothing'.
--   The result of the action is ignored.
withOutRounded_ :: Precision p => (Ptr MPFR -> IO a) -> IO (Maybe (Rounded r p))
withOutRounded_ = fmap fst . withOutRounded

-- | Like 'withOutRounded' but with the limbs allocated by GHC, which should be
--   slightly faster.  However, it will crash if MPFR tries to reallocate the
--   limbs, so the action must not try to change the precision or clear it, etc.
unsafeWithOutRounded :: Precision p => (Ptr MPFR -> IO a) -> IO (Maybe (Rounded r p), a)
unsafeWithOutRounded = out_

-- | Like 'withOutRounded_' but with the limbs allocated by GHC, which should be
--   slightly faster.  However, it will crash if MPFR tries to reallocate the
--   limbs, so the action must not try to change the precision or clear it, etc.
unsafeWithOutRounded_ :: Precision p => (Ptr MPFR -> IO a) -> IO (Maybe (Rounded r p))
unsafeWithOutRounded_ = fmap fst . out_

-- | Allocates and initializes a new @mpfr_t@ to the value.  If the precision matches after
--   the action then it is peeked and returned.  Otherwise you get 'Nothing'.
withInOutRounded :: Precision p => Rounded r p -> (Ptr MPFR -> IO a) -> IO (Maybe (Rounded r p), a)
-- FIXME: optimize to reduce copying
withInOutRounded i f =
  withOutRounded $ \ofr ->
    in_ i $ \ifr -> do
      _ <- mpfr_set ofr ifr (fromIntegral (fromEnum TowardNearest))
      f ofr

-- | Allocates and initializes a new @mpfr_t@ to the value.  If the precision matches after
--   the action then it is peeked and returned.  Otherwise you get 'Nothing'.  The result
--   ot the action is ignored.
withInOutRounded_ :: Precision p => Rounded r p -> (Ptr MPFR -> IO a) -> IO (Maybe (Rounded r p))
withInOutRounded_ x = fmap fst . withInOutRounded x

-- | Peek an @mpfr_t@ at its actual precision, reified.
peekRounded :: Rounding r => Ptr MPFR -> (forall (p :: *) . Precision p => Rounded r p -> IO a) -> IO a
peekRounded ptr f = do
  MPFR{ mpfrPrec = p', mpfrSign = s', mpfrExp = e', mpfrD = d' } <- peek ptr
  asByteArray d' (precBytes p') $ \l' -> reifyPrecision (fromIntegral p') (wrap f (Rounded p' s' e' l'))
  where
    wrap :: forall (p :: *) (r :: RoundingMode) (a :: *) . (Rounding r, Precision p) => (forall (q :: *) . Precision q => Rounded r q -> IO a) -> Rounded r p -> Proxy p -> IO a
    wrap g r = \_proxy -> g r


-- "The number of limbs in use is controlled by _mpfr_prec, namely ceil(_mpfr_prec/mp_bits_per_limb)."
-- <http://www.mpfr.org/mpfr-current/mpfr.html#Internals>
precBytes :: MPFRPrec -> Int
precBytes prec = bytesPerLimb * ((fromIntegral prec + bitsPerLimb1) `div` bitsPerLimb)
bytesPerLimb :: Int
bytesPerLimb = sizeOf (undefined :: MPLimb)
bitsPerLimb :: Int
bitsPerLimb = bytesPerLimb * 8
bitsPerLimb1 :: Int
bitsPerLimb1 = bitsPerLimb - 1


erangeBit :: Int
erangeBit = 5 -- sync with cbits/wrappers.c

rnd :: Rounding r => Rounded r p -> MPFRRnd
rnd = fromIntegral . fromEnum . rounding . proxyRounding

proxyRounding :: Rounded r p -> Proxy r
proxyRounding _ = Proxy

withByteArray :: ByteArray# -> (Ptr a -> Int -> IO r) -> IO r
withByteArray ba# f = do
  let bytes = I# (sizeofByteArray# ba#)
  allocaBytes bytes $ \ptr@(Ptr addr#) -> do
    IO (\s -> (# copyByteArrayToAddr# ba# 0# addr# (sizeofByteArray# ba#) s, () #))
    f ptr bytes

asByteArray :: Ptr a -> Int -> (ByteArray# -> IO r) -> IO r
asByteArray (Ptr addr#) (I# bytes#) f = do
  IO $ \s# -> case newByteArray# bytes# s# of
    (# s'#, mba# #) ->
      case unsafeFreezeByteArray# mba# (copyAddrToByteArray# addr# mba# 0# bytes# s'#) of
        (# s''#, ba# #) -> case f ba# of IO r -> r s''#
