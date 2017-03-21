{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RoleAnnotations #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Rounded
-- Copyright   :  (C) 2012-2014 Edward Kmett, Daniel Peebles
--                (C) 2013-2017 Claude Heiland-Allen
-- License     :  LGPL
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Numeric.Rounded
    (
    -- * floating point numbers with a specified rounding mode and precision
      Rounded(..)
    , fromInt
    , fromDouble
    , toDouble
    , precRound
    -- * Precision
    , Precision(precision)
    , Bytes
    , reifyPrecision
    -- * Rounding
    , Rounding(rounding)
    , RoundingMode(..)
    , reifyRounding
    -- * Useful Constants
    , kLog2
    , kEuler
    , kCatalan
    -- * Combinators that are oblivious to precision
    , (.+.)
    , (.-.)
    , (.*.)
    , abs'
    , negate'
    , decodeFloat'
    , succUlp
    , predUlp
    -- * Mixed-precision operations
    , (!+!)
    , (!-!)
    , (!*!)
    , abs_
    , negate_
    , (!/!)
    , (!==!)
    , (!/=!)
    , compare_
    , (!>=!)
    , (!<=!)
    , (!>!)
    , (!<!)
    , min_
    , max_
    , sqrt_
    , exp_
    , log_
    , sin_
    , tan_
    , cos_
    , asin_
    , atan_
    , acos_
    , sinh_
    , tanh_
    , cosh_
    , asinh_
    , atanh_
    , acosh_
    -- * Foreign Function Interface
    , withInRounded
    , withInOutRounded
    , withOutRounded
    , peekRounded
    ) where

import Control.Exception (bracket, bracket_)
import Data.Bits (shiftL)
import Data.Int (Int32)
import Data.Proxy (Proxy(..))
import Data.Ratio ((%))
import Data.Tuple (swap)
import Numeric (showFloat)

import Foreign (with, alloca, allocaBytes, peek, sizeOf, nullPtr)
import Foreign.C (CInt(..), CIntMax(..), CSize(..), CChar(..))
import Foreign.C.String (peekCString)

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

import Numeric.GMP.Utils (withInInteger, withOutInteger, withInRational)
import Numeric.GMP.Types (MPZ, MPQ, MPLimb, MPExp(..))

import Numeric.MPFR.Types

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

foreign import ccall unsafe "mpfr_get_d" mpfr_get_d :: Ptr MPFR -> MPFRRnd -> IO Double

-- | Round to Double with the given rounding mode.
toDouble :: (Rounding r, Precision p) => Rounded r p -> Double
toDouble x = unsafePerformIO $ withInRounded x $ \xfr -> mpfr_get_d xfr (rnd x)
-- this syntax is strange, but it seems to be the way it works...
{-# RULES "realToFrac/toDouble" forall (x :: (Rounding r, Precision p) => Rounded r p) . realToFrac x = toDouble x #-}

-- | Round to a different precision with the given rounding mode.
precRound :: (Rounding r, Precision p1, Precision p2) => Rounded r p1 -> Rounded r p2
precRound x = unsafePerformIO $ do
  (Just y, _) <- withInRounded x $ \xfr -> withOutRounded $ \yfr ->
    mpfr_set yfr xfr (rnd x)
  return y
-- TODO figure out correct syntax (if even possible) to allow RULE
-- {-# RULES "realToFrac/precRound" realToFrac = precRound #-}

foreign import ccall unsafe "mpfr_get_str" mpfr_get_str :: Ptr CChar -> Ptr MPFRExp -> Int -> CSize -> Ptr MPFR -> MPFRRnd -> IO (Ptr CChar)
foreign import ccall unsafe "mpfr_free_str" mpfr_free_str :: Ptr CChar -> IO ()

toString :: (Rounding r, Precision p) => Rounded r p -> String
-- FIXME: what do about unsightly 0.1 -> 0.1000...0002 or 9.999...9995e-2 issues
toString x = unsafePerformIO $ do
  (s, e) <- withInRounded x $ \xfr -> with 0 $ \eptr -> do
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
  showsPrec p x = showParen (p >= 7) (toString x ++)


type Unary = Ptr MPFR -> Ptr MPFR -> MPFRRnd -> IO CInt

foreign import ccall unsafe "mpfr_set" mpfr_set :: Unary
foreign import ccall unsafe "mpfr_abs" mpfr_abs :: Unary
foreign import ccall unsafe "mpfr_neg" mpfr_neg :: Unary
foreign import ccall unsafe "mpfr_log" mpfr_log :: Unary
foreign import ccall unsafe "mpfr_exp" mpfr_exp :: Unary
foreign import ccall unsafe "mpfr_sqrt" mpfr_sqrt :: Unary
foreign import ccall unsafe "mpfr_sin" mpfr_sin :: Unary
foreign import ccall unsafe "mpfr_cos" mpfr_cos :: Unary
foreign import ccall unsafe "mpfr_tan" mpfr_tan :: Unary
foreign import ccall unsafe "mpfr_asin" mpfr_asin :: Unary
foreign import ccall unsafe "mpfr_acos" mpfr_acos :: Unary
foreign import ccall unsafe "mpfr_atan" mpfr_atan :: Unary
foreign import ccall unsafe "mpfr_sinh" mpfr_sinh :: Unary
foreign import ccall unsafe "mpfr_cosh" mpfr_cosh :: Unary
foreign import ccall unsafe "mpfr_tanh" mpfr_tanh :: Unary
foreign import ccall unsafe "mpfr_asinh" mpfr_asinh :: Unary
foreign import ccall unsafe "mpfr_acosh" mpfr_acosh :: Unary
foreign import ccall unsafe "mpfr_atanh" mpfr_atanh :: Unary

unary
  :: (Rounding r, Precision p1, Precision p2)
  => Unary -> Rounded r p1 -> Rounded r p2
unary f a = unsafePerformIO $ do
  (Just c, _) <- withInRounded a $ \afr ->
    withOutRounded_ $ \cfr ->
      f cfr afr (rnd a)
  return c

unary' :: Rounding r => Unary -> Rounded r p -> Rounded r p
unary' f a = unsafePerformIO $ do
  (Just c, _) <- withInRounded a $ \afr ->
    withOutRounded_' (roundedPrec a) $ \cfr ->
      f cfr afr (rnd a)
  return c

unary'' :: Unary -> Rounded r p -> Rounded r p
unary'' f a = unsafePerformIO $ do
  (Just c, _) <- withInRounded a $ \afr ->
    withOutRounded_' (roundedPrec a) $ \cfr ->
      f cfr afr (fromIntegral (fromEnum TowardNearest))
  return c

abs' :: Rounded r p -> Rounded r p
abs' = unary'' mpfr_abs

negate' :: Rounding r => Rounded r p -> Rounded r p
negate' = unary' mpfr_neg

(.-.), (.+.), (.*.) :: Rounding r => Rounded r p -> Rounded r p -> Rounded r p
(.-.) = binary' mpfr_sub
(.+.) = binary' mpfr_add
(.*.) = binary' mpfr_mul

infixl 6 .+., .-.
infixl 7 .*.


abs_, negate_, log_, exp_, sqrt_,
 sin_, cos_, tan_, asin_, acos_, atan_,
   sinh_, cosh_, tanh_, asinh_, acosh_, atanh_
  :: (Rounding r, Precision p1, Precision p2)
  => Rounded r p1 -> Rounded r p2
abs_ = unary mpfr_abs
negate_ = unary mpfr_neg
log_ = unary mpfr_log
exp_ = unary mpfr_exp
sqrt_ = unary mpfr_sqrt
sin_ = unary mpfr_sin
cos_ = unary mpfr_cos
tan_ = unary mpfr_tan
asin_ = unary mpfr_asin
acos_ = unary mpfr_acos
atan_ = unary mpfr_atan
sinh_ = unary mpfr_sinh
cosh_ = unary mpfr_cosh
tanh_ = unary mpfr_tanh
asinh_ = unary mpfr_asinh
acosh_ = unary mpfr_acosh
atanh_ = unary mpfr_atanh

type Binary = Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> MPFRRnd -> IO CInt

foreign import ccall unsafe "mpfr_add" mpfr_add :: Binary
foreign import ccall unsafe "mpfr_sub" mpfr_sub :: Binary
foreign import ccall unsafe "mpfr_mul" mpfr_mul :: Binary
foreign import ccall unsafe "mpfr_div" mpfr_div :: Binary
foreign import ccall unsafe "mpfr_min" mpfr_min :: Binary
foreign import ccall unsafe "mpfr_max" mpfr_max :: Binary
foreign import ccall unsafe "mpfr_atan2" mpfr_atan2 :: Binary

binary
  :: (Rounding r, Precision p1, Precision p2, Precision p3)
  => Binary -> Rounded r p1 -> Rounded r p2 -> Rounded r p3
binary f a b = unsafePerformIO $ do
  (Just c, _) <- withInRounded a $ \afr ->
    withInRounded b $ \bfr ->
      withOutRounded_ $ \cfr ->
        f cfr afr bfr (rnd a)
  return c

min_, max_, (!+!), (!-!), (!*!), (!/!), atan2_
  :: (Rounding r, Precision p1, Precision p2, Precision p3)
  => Rounded r p1 -> Rounded r p2 -> Rounded r p3
min_ = binary mpfr_min
max_ = binary mpfr_max
(!+!) = binary mpfr_add
(!-!) = binary mpfr_sub
(!*!) = binary mpfr_mul
(!/!) = binary mpfr_div
atan2_ = binary mpfr_atan2

infixl 6 !+!, !-!
infixl 7 !*!, !/!

binary' :: Rounding r => Binary -> Rounded r p -> Rounded r p -> Rounded r p
binary' f a b = unsafePerformIO $ do
  (Just c, _) <- withInRounded a $ \afr ->
    withInRounded b $ \bfr ->
      withOutRounded_' (roundedPrec a) $ \cfr ->
        f cfr afr bfr (rnd a)
  return c

type Comparison = Ptr MPFR -> Ptr MPFR -> IO CInt

foreign import ccall unsafe "mpfr_cmp"            mpfr_cmp            :: Comparison
foreign import ccall unsafe "mpfr_equal_p"        mpfr_equal_p        :: Comparison
foreign import ccall unsafe "mpfr_lessgreater_p"  mpfr_lessgreater_p  :: Comparison
foreign import ccall unsafe "mpfr_less_p"         mpfr_less_p         :: Comparison
foreign import ccall unsafe "mpfr_greater_p"      mpfr_greater_p      :: Comparison
foreign import ccall unsafe "mpfr_lessequal_p"    mpfr_lessequal_p    :: Comparison
foreign import ccall unsafe "mpfr_greaterequal_p" mpfr_greaterequal_p :: Comparison

cmp' :: Comparison -> Rounded r p1 -> Rounded r p2 -> CInt
cmp' f a b = unsafePerformIO $
  withInRounded a $ \afr ->
  withInRounded b $ \bfr -> do
  f afr bfr

cmp :: Comparison -> Rounded r p1 -> Rounded r p2 -> Bool
cmp f a b = cmp' f a b /= 0

(!==!), (!/=!), (!<=!), (!>=!), (!<!), (!>!)
  :: (Precision p1, Precision p2)
  => Rounded r p1 -> Rounded r p2 -> Bool
(!==!) = cmp mpfr_equal_p
(!/=!) = cmp mpfr_lessgreater_p
(!<=!) = cmp mpfr_lessequal_p
(!>=!) = cmp mpfr_greaterequal_p
(!<!) = cmp mpfr_less_p
(!>!) = cmp mpfr_greater_p

infix 4 !==!, !/=!, !<=!, !>=!, !<!, !>!

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

foreign import ccall unsafe "mpfr_set_z" mpfr_set_z :: Ptr MPFR -> Ptr MPZ -> MPFRRnd -> IO CInt
foreign import ccall unsafe "mpfr_sgn" mpfr_sgn :: Ptr MPFR -> IO CInt

sgn :: (Rounding r, Precision p) => Rounded r p -> Ordering
sgn x = compare (unsafePerformIO $ withInRounded x mpfr_sgn) 0

instance (Rounding r, Precision p) => Num (Rounded r p) where
  (+) = (.+.)
  (-) = (.-.)
  (*) = (.*.)
  negate = negate'
  fromInteger j = r where  -- TODO restore small integer optimisation
    r = unsafePerformIO $ do
          (Just x, _) <- withInInteger j $ \jz -> withOutRounded_ $ \jfr -> mpfr_set_z jfr jz (rnd r)
          return x
  abs = abs'
  signum x = case sgn x of
    LT -> -1
    EQ -> 0
    GT -> 1

foreign import ccall unsafe "mpfr_set_q" mpfr_set_q :: Ptr MPFR -> Ptr MPQ -> MPFRRnd -> IO CInt

instance (Rounding r, Precision p) => Fractional (Rounded r p) where
  fromRational q = r where -- TODO small integer optimisation
    r = unsafePerformIO $ do
          (Just x, _) <- withInRational q $ \qq -> withOutRounded_ $ \qfr -> mpfr_set_q qfr qq (rnd r)
          return x
  (/) = (!/!)

foreign import ccall unsafe "__gmpfr_set_sj" mpfr_set_sj :: Ptr MPFR -> CIntMax -> MPFRRnd -> IO CInt

-- | Construct a properly rounded floating point number from an 'Int'.
fromInt :: (Rounding r, Precision p) => Int -> Rounded r p
fromInt i = r
  where
    r = unsafePerformIO $ do
      (Just x, _) <- withOutRounded_ $ \xfr -> mpfr_set_sj xfr (fromIntegral i) (rnd r)
      return x
-- TODO figure out correct syntax (if even possible) to allow RULE
-- {-# RULES "fromIntegral/fromInt" fromIntegral = fromInt #-}

foreign import ccall unsafe "mpfr_set_d" mpfr_set_d :: Ptr MPFR -> Double -> MPFRRnd -> IO CInt

-- | Construct a rounded floating point number directly from a 'Double'.
fromDouble :: (Rounding r, Precision p) => Double -> Rounded r p
fromDouble d = r
  where
    r = unsafePerformIO $ do
      (Just x, _) <- withOutRounded_ $ \xfr -> mpfr_set_d xfr d (rnd r)
      return x
-- TODO figure out correct syntax (if even possible) to allow RULE
-- {-# RULES "realToFrac/fromDouble" realToFrac = fromDouble #-}


foreign import ccall unsafe "mpfr_nextabove" mpfr_nextabove :: Ptr MPFR -> IO ()
foreign import ccall unsafe "mpfr_nextbelow" mpfr_nextbelow :: Ptr MPFR -> IO ()

inplace :: (Ptr MPFR -> IO ()) -> Rounded r p -> Rounded r p
inplace f y = unsafePerformIO $ do
  (Just x, _) <- withOutRounded_' (roundedPrec y) $ \xfr -> withInRounded y $ \yfr -> do
    _ <- mpfr_set xfr yfr (fromIntegral (fromEnum TowardNearest))
    f xfr
  return x

succUlp, predUlp :: Rounded r p -> Rounded r p
succUlp = inplace mpfr_nextabove
predUlp = inplace mpfr_nextbelow

type Constant = Ptr MPFR -> MPFRRnd -> IO CInt

constant :: (Rounding r, Precision p) => Constant -> Rounded r p
constant k = r where
  r = unsafePerformIO $ do
    (Just x, _) <- withOutRounded_ $ \xfr -> k xfr (rnd r)
    return x


instance (Rounding r, Precision p) => Floating (Rounded r p) where
  pi    = kPi
  exp   = exp_
  sqrt  = sqrt_
  log   = log_
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

toRational' :: Precision p => Rounded r p -> Rational
toRational' r
   | e > 0     = fromIntegral (s `shiftL` e)
   | otherwise = s % (1 `shiftL` negate e)
   where (s, e) = decodeFloat' r

instance (Rounding r, Precision p) => Real (Rounded r p) where
  toRational = toRational'

instance (Rounding r, Precision p) => RealFrac (Rounded r p) where
  -- FIXME: properFraction goes via Rational, needs optimization
  properFraction r = (i, fromRational f) where
    (i, f) = properFraction (toRational r)


type Test = Ptr MPFR -> IO CInt

tst :: (Precision p) => Test -> Rounded r p -> Bool
tst f x = unsafePerformIO $ withInRounded x $ \xfr -> do
  t <- f xfr
  return (t /= 0)

foreign import ccall unsafe "mpfr_nan_p" mpfr_nan_p :: Test
foreign import ccall unsafe "mpfr_inf_p" mpfr_inf_p :: Test
foreign import ccall unsafe "mpfr_zero_p" mpfr_zero_p :: Test
foreign import ccall unsafe "mpfr_signbit" mpfr_signbit :: Test

foreign import ccall unsafe "mpfr_get_z_2exp" mpfr_get_z_2exp :: Ptr MPZ -> Ptr MPFR -> IO MPExp
foreign import ccall unsafe "mpfr_set_z_2exp" mpfr_set_z_2exp :: Ptr MPFR -> Ptr MPZ -> MPFRExp -> MPFRRnd -> IO CInt


decodeFloat' :: Rounded r p -> (Integer, Int)
decodeFloat' x = unsafePerformIO $ do
  withInRounded x $ \xfr -> withOutInteger $ \xz -> do
    e <- mpfr_get_z_2exp xz xfr -- FIXME sets error flags, need to wrap...
    return (fromIntegral e)

encodeFloat' :: (Rounding r, Precision p) => Integer -> Int -> Rounded r p
encodeFloat' j e = r where
  r = unsafePerformIO $ do
        (Just x, _) <- withInInteger j $ \jz -> withOutRounded_ $ \xfr -> mpfr_set_z_2exp xfr jz (fromIntegral e) (rnd r)
        return x

instance (Rounding r, Precision p) => RealFloat (Rounded r p) where
  floatRadix  _ = 2
  floatDigits = self where
    self _ = p
    p = precision (0 `asTypeIn` self)
    asTypeIn :: a -> (a -> b) -> a
    asTypeIn = const

  -- FIXME: this should do for now, but the real ones can change...
  floatRange _ = (fromIntegral (minBound :: Int32), fromIntegral (maxBound :: Int32))

  decodeFloat = decodeFloat'
  encodeFloat = encodeFloat'
  isNaN = tst mpfr_nan_p
  isInfinite = tst mpfr_inf_p
  isDenormalized _ = False
  isNegativeZero r = tst mpfr_zero_p r && tst mpfr_signbit r
  isIEEE _ = True -- is this a lie? it mostly behaves like an IEEE float, despite being much bigger
  atan2 = atan2_

foreign import ccall unsafe "mpfr_const_pi" mpfr_const_pi :: Constant
foreign import ccall unsafe "mpfr_const_log2" mpfr_const_log2 :: Constant
foreign import ccall unsafe "mpfr_const_euler" mpfr_const_euler :: Constant
foreign import ccall unsafe "mpfr_const_catalan" mpfr_const_catalan :: Constant

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


withInRounded' :: Rounded r p -> (MPFR -> IO a) -> IO a
withInRounded' (Rounded p s e l) f = withByteArray l $ \ptr _bytes -> f MPFR
  { mpfrPrec = p
  , mpfrSign = s
  , mpfrExp = e
  , mpfrD = ptr
  }

-- | Use a Rounded as a /constant/ @mpfr_t@ (attempts to modify it may explode,
--   changing the precision will explode).
withInRounded :: Rounded r p -> (Ptr MPFR -> IO a) -> IO a
withInRounded x f = withInRounded' x $ \y -> with y f


foreign import ccall unsafe "mpfr_init2"
  mpfr_init2 :: Ptr MPFR -> MPFRPrec -> IO ()

foreign import ccall unsafe "mpfr_clear"
  mpfr_clear :: Ptr MPFR -> IO ()

-- | f mustn't change the precision or mpfr will explode by reallocating limbs
--   it didn't allocate itself...
withOutRounded_' :: MPFRPrec -> (Ptr MPFR -> IO a) -> IO (Maybe (Rounded r p), a)
withOutRounded_' p f = allocaBytes (precBytes p) $ \d -> with
  MPFR{ mpfrPrec = p, mpfrSign = 0, mpfrExp = 0, mpfrD = d } $ \ptr -> do
  a <- f ptr
  MPFR{ mpfrPrec = p', mpfrSign = s', mpfrExp = e', mpfrD = d' } <- peek ptr
  if p /= p' then return (Nothing, a) else
    asByteArray d' (precBytes p') $ \l' -> return (Just (Rounded p' s' e' l'), a)

withOutRounded_ :: Precision p => (Ptr MPFR -> IO a) -> IO (Maybe (Rounded r p), a)
withOutRounded_ f = r where
  r = withOutRounded_' prec f
  prec = fromIntegral (precision (t r))
  t :: IO (Maybe t, a) -> t
  t _ = undefined

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

-- | Allocates and initializes a new @mpfr_t@ to the value.  If the precision matches after
--   the action then it is peeked and returned.  Otherwise you get 'Nothing'.
withInOutRounded :: Precision p => Rounded r p -> (Ptr MPFR -> IO a) -> IO (Maybe (Rounded r p), a)
-- FIXME: optimize to reduce copying
withInOutRounded i f =
  withOutRounded $ \ofr ->
    withInRounded i $ \ifr -> do
      mpfr_set ofr ifr (fromIntegral (fromEnum TowardNearest)) 
      f ofr

-- | Peek an @mpfr_t@ with reified precision.
peekRounded :: Rounding r => Ptr MPFR -> (forall (p :: *) . Precision p => Rounded r p -> IO a) -> IO a
peekRounded ptr f = do
  MPFR{ mpfrPrec = p', mpfrSign = s', mpfrExp = e', mpfrD = d' } <- peek ptr
  asByteArray d' (precBytes p') $ \l' -> reifyPrecision (fromIntegral p') (wrap f (Rounded p' s' e' l'))
  where
    wrap :: forall (p :: *) (r :: RoundingMode) (a :: *) . (Rounding r, Precision p) => (forall (q :: *) . Precision q => Rounded r q -> IO a) -> Rounded r p -> Proxy p -> IO a
    wrap f r = \proxy -> f r


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
