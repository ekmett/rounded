{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
module Numeric.Rounded.Simple
  ( RoundingMode(..)
  , Precision
  , Rounded()
  , reifyRounded
  , simplify
  , Constant
  , Unary
  , Unary'
  , Binary
  -- * conversion
  , toDouble
  , toInteger'
  , precRound
  -- * constants
  , kPi
  , kLog2
  , kEuler
  , kCatalan
  -- * Ord
  , min_
  , max_
  -- * Num
  , add_
  , sub_
  , mul_
  , negate_
  , abs_
-- , signum_
  , fromInteger'
  -- * Fractional
  , div_
-- , recip_
  , fromRational'
  -- * Real
  , toRational'
  -- * Floating
  , log_
  , exp_
  , sqrt_
  , sin_
  , cos_
  , tan_
  , asin_
  , acos_
  , atan_
  , sinh_
  , cosh_
  , tanh_
  , asinh_
  , acosh_
  , atanh_
  , log1p_
  , expm1_
  -- * RealFloat
  , atan2_
  , floatRadix'
  , floatDigits'
  , floatRange'
  , decodeFloat'
  , encodeFloat'
  , exponent'
  , significand'
  , scaleFloat'
  , isNaN'
  , isInfinite'
  , isDenormalized'
  , isNegativeZero'
  , isIEEE'
  -- * Show
  , show'
  -- * Read
  , read'
  -- * Foreign Function Interface
  , withInRounded
  , withInOutRounded
  , withInOutRounded_
  , withOutRounded
  , withOutRounded_
  , peekRounded
  ) where

import Control.Exception (bracket_)
import Foreign (Ptr(..), alloca)
import GHC.Prim ( ByteArray# )

import Numeric.MPFR.Types
import Numeric.MPFR.Raw.Unsafe (mpfr_init2, mpfr_clear, mpfr_set)
import qualified Numeric.Rounded as R
import Numeric.Rounded.Rounding

type Precision = Int

-- | A properly rounded floating-point number with a given rounding mode and precision.
data Rounded = Rounded
  { roundedPrec  :: !MPFRPrec
  , _roundedSign  :: !MPFRSign
  , _roundedExp   :: !MPFRExp
  , _roundedLimbs :: !ByteArray#
  }

precision :: Rounded -> Int
precision = fromIntegral . roundedPrec

reifyRounded :: Rounded -> (forall p . R.Precision p => R.Rounded r p -> a) -> a
reifyRounded (Rounded p s e l) f = R.reifyPrecision (fromIntegral p) (\q -> f (g q (R.Rounded p s e l)))
  where
    g :: R.Precision q => proxy q -> R.Rounded s q -> R.Rounded s q
    g _ x = x

simplify :: R.Rounded r p -> Rounded
simplify (R.Rounded p s e l) = Rounded p s e l

type Constant = RoundingMode -> Precision -> Rounded

constant :: (forall r p . (R.Rounding r, R.Precision p) => R.Rounded r p) -> Constant
constant f r q = R.reifyRounding r (\pr -> R.reifyPrecision q (\pq -> g pr pq f))
  where
    g :: (R.Rounding r, R.Precision p) => proxy1 r -> proxy2 p -> R.Rounded r p -> Rounded
    g _ _ b = simplify b

kPi, kLog2, kEuler, kCatalan :: Constant

kPi = constant pi
kLog2 = constant R.kLog2
kEuler = constant R.kEuler
kCatalan = constant R.kCatalan

type Unary = RoundingMode -> Precision -> Rounded -> Rounded

unary :: (forall r p q . (R.Rounding r, R.Precision p, R.Precision q) => R.Rounded r p -> R.Rounded r q) -> Unary
unary f r q a = R.reifyRounding r (\pr -> R.reifyPrecision q (\pq -> reifyRounded a (\ra -> g pr pq f ra)))
  where
    g :: (R.Rounding r, R.Precision p, R.Precision q) => proxy1 r -> proxy2 q -> (R.Rounded r p -> R.Rounded r q) -> R.Rounded r p -> Rounded
    g _ _ h b = simplify (h b)

abs_, negate_, log_, exp_, sqrt_,
 sin_, cos_, tan_, asin_, acos_, atan_,
  sinh_, cosh_, tanh_, asinh_, acosh_, atanh_,
   log1p_, expm1_,
     precRound :: Unary

abs_ = unary R.abs_
negate_ = unary R.negate_
log_ = unary R.log_
exp_ = unary R.exp_
sqrt_ = unary R.sqrt_
sin_ = unary R.sin_
cos_ = unary R.cos_
tan_ = unary R.tan_
asin_ = unary R.asin_
acos_ = unary R.acos_
atan_ = unary R.atan_
sinh_ = unary R.sinh_
cosh_ = unary R.cosh_
tanh_ = unary R.tanh_
asinh_ = unary R.asinh_
acosh_ = unary R.acosh_
atanh_ = unary R.atanh_
log1p_ = unary R.log1p_
expm1_ = unary R.expm1_
precRound = unary R.precRound

fromInteger' :: RoundingMode -> Precision -> Integer -> Rounded
fromInteger' r p n = R.reifyRounding r (\pr -> R.reifyPrecision p (\pp -> g pr pp (fromInteger n)))
  where
    g :: (R.Rounding r, R.Precision p) => proxy1 r -> proxy2 p -> R.Rounded r p -> Rounded
    g _ _ x = simplify x

fromRational' :: RoundingMode -> Precision -> Rational -> Rounded
fromRational' r p n = R.reifyRounding r (\pr -> R.reifyPrecision p (\pp -> g pr pp (fromRational n)))
  where
    g :: (R.Rounding r, R.Precision p) => proxy1 r -> proxy2 p -> R.Rounded r p -> Rounded
    g _ _ x = simplify x

type Binary = RoundingMode -> Precision -> Rounded -> Rounded -> Rounded

binary :: (forall r p q pq . (R.Rounding r, R.Precision p, R.Precision q, R.Precision pq) => R.Rounded r p -> R.Rounded r q -> R.Rounded r pq) -> Binary
binary f r pq a b = R.reifyRounding r (\pr -> R.reifyPrecision pq (\ppq -> reifyRounded a (\ra -> reifyRounded b (\rb -> g pr ppq f ra rb))))
  where
    g :: (R.Rounding r, R.Precision p, R.Precision q, R.Precision pq) => proxy1 r -> proxy2 pq -> (R.Rounded r p -> R.Rounded r q -> R.Rounded r pq) -> R.Rounded r p -> R.Rounded r q -> Rounded
    g _ _ h x y = simplify (h x y)

binary' :: (forall r p q pq . (R.Rounding r, R.Precision p, R.Precision q, R.Precision pq) => R.Rounded r p -> R.Rounded r q -> R.Rounded r pq) -> Rounded -> Rounded -> Rounded
binary' f a b = binary f R.TowardNearest (precision a `max` precision b) a b

min_, max_, add_, sub_, mul_, div_, atan2_ :: Binary

min_ = binary R.min_
max_ = binary R.max_
add_ = binary (R.!+!)
sub_ = binary (R.!-!)
mul_ = binary (R.!*!)
div_ = binary (R.!/!)
atan2_ = binary R.atan2_

type Unary' a = RoundingMode -> Rounded -> a

unary' :: (forall r p . (R.Rounding r, R.Precision p) => R.Rounded r p -> a) -> Unary' a
unary' f r a = R.reifyRounding r (\pr -> reifyRounded a (\ra -> g pr f ra))
  where
    g :: (R.Rounding r, R.Precision p) => proxy r -> (R.Rounded r p -> a) -> R.Rounded r p -> a
    g _ h x = h x

unary'' :: (forall r p . (R.Rounding r, R.Precision p) => R.Rounded r p -> a) -> Rounded -> a
unary'' f a = unary' f R.TowardNearest a

toDouble :: Unary' Double
toDouble = unary' R.toDouble

toInteger' :: Unary' Integer
toInteger' = unary' R.toInteger'

-- Real

toRational' :: Unary' Rational
toRational' = unary' toRational

-- RealFloat

floatRadix' :: Rounded -> Integer
floatRadix' = unary'' floatRadix

floatDigits' :: Rounded -> Int
floatDigits' = unary'' floatDigits

floatRange' :: Rounded -> (Int, Int)
floatRange' = unary'' floatRange

decodeFloat' :: Rounded -> (Integer, Int)
decodeFloat' = unary'' decodeFloat

encodeFloat' :: RoundingMode -> Precision -> Integer -> Int -> Rounded
encodeFloat' r p m e = R.reifyRounding r (\rp -> R.reifyPrecision p (\pp -> g rp pp (encodeFloat m e)))
  where
    g :: R.Precision p => proxy1 r -> proxy2 p -> R.Rounded r p -> Rounded
    g _ _ x = simplify x

exponent' :: Rounded -> Int
exponent' = unary'' exponent

significand' :: Rounded -> Rounded
significand' = unary'' (\a -> simplify (significand a))

scaleFloat' :: Int -> Rounded -> Rounded
scaleFloat' n = unary'' (\a -> simplify (scaleFloat n a))

isNaN' :: Rounded -> Bool
isNaN' = unary'' isNaN

isInfinite' :: Rounded -> Bool
isInfinite' = unary'' isInfinite

isDenormalized' :: Rounded -> Bool
isDenormalized' = unary'' isDenormalized

isNegativeZero' :: Rounded -> Bool
isNegativeZero' = unary'' isNegativeZero

isIEEE' :: Rounded -> Bool
isIEEE' = unary'' isIEEE

{-
-- RealFrac -- FIXME TODO
  properFraction :: Integral b => a -> (b, a)
  truncate :: Integral b => a -> b
  round :: Integral b => a -> b
  ceiling :: Integral b => a -> b
  floor :: Integral b => a -> b
  modf
-}

type Comparison = Rounded -> Rounded -> Bool

cmp :: (forall p q . (R.Precision p, R.Precision q) => R.Rounded R.TowardNearest p -> R.Rounded R.TowardNearest q -> Bool) -> Comparison
cmp f a b = reifyRounded a (\ra -> reifyRounded b (\rb -> f ra rb))

instance Eq Rounded where
  (==) = cmp (R.!==!)
  (/=) = cmp (R.!/=!)

instance Ord Rounded where
  compare a b = reifyRounded a (\ra -> reifyRounded b (\rb -> R.compare_ ra rb))
  (<) = cmp (R.!<!)
  (<=) = cmp (R.!<=!)
  (>) = cmp (R.!>!)
  (>=) = cmp (R.!>=!)
  max = binary' R.max_
  min = binary' R.min_

-- Show

show' :: Rounded -> String
show' = unary'' show

-- Read

read' :: RoundingMode -> Precision -> String -> Rounded
read' r p s = R.reifyRounding r (\pr -> R.reifyPrecision p (\pp -> g pr pp (read s)))
  where
    g :: (R.Rounding r, R.Precision p) => proxy1 r -> proxy2 p -> R.Rounded r p -> Rounded
    g _ _ x = simplify x

-- Foreign Function Interface

-- | Use a value as a /constant/ @mpfr_t@ (attempts to modify it may explode,
--   changing the precision will explode).
withInRounded :: Rounded -> (Ptr MPFR -> IO a) -> IO a
withInRounded a f = reifyRounded a (\ra -> R.withInRounded ra f)

-- | Allocates and initializes a new @mpfr_t@, after the action it is peeked
--   and returned.
withOutRounded :: Precision -> (Ptr MPFR -> IO a) -> IO (Rounded, a)
withOutRounded prec f = r where
  r = alloca $ \ptr -> bracket_ (mpfr_init2 ptr (fromIntegral prec)) (mpfr_clear ptr) $ do
    a <- f ptr
    m <- peekRounded ptr
    return (m, a)

-- | Allocates and initializes a new @mpfr_t@, after the action it is peeked
--   and returned.
--   The result of the action is ignored.
withOutRounded_ :: Precision -> (Ptr MPFR -> IO a) -> IO Rounded
withOutRounded_ p = fmap fst . withOutRounded p

-- | Allocates and initializes a new @mpfr_t@ to the value.  After the action
--   it is peeked and returned.
withInOutRounded :: Rounded -> (Ptr MPFR -> IO a) -> IO (Rounded, a)
-- FIXME: optimize to reduce copying
withInOutRounded i f =
  withOutRounded (fromIntegral (roundedPrec i)) $ \ofr ->
    withInRounded i $ \ifr -> do
      _ <- mpfr_set ofr ifr (fromIntegral (fromEnum TowardNearest))
      f ofr

-- | Allocates and initializes a new @mpfr_t@ to the value.  After the action
--   it is peeked and returned.
--   The result of the action is ignored.
withInOutRounded_ :: Rounded -> (Ptr MPFR -> IO a) -> IO Rounded
withInOutRounded_ x = fmap fst . withInOutRounded x

-- | Peek an @mpfr_t@ at its actual precision, reified.
peekRounded :: Ptr MPFR -> IO Rounded
peekRounded ptr = R.peekRounded ptr f
  where
    f :: R.Precision p => R.Rounded TowardNearest p -> IO Rounded
    f mr = return (simplify mr)
