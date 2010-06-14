{-# LANGUAGE CPP, ScopedTypeVariables, MagicHash, EmptyDataDecls, FlexibleContexts, MultiParamTypeClasses #-}
module Numeric.Precision.Fixed 
    ( Fixed(..)
    , RoundMode(..)
    , Near, Zero, Up, Down
    , Precision
    , reflectMode
    , reflectPrecision
    , fromMPFR
    , fromInt
    , fromWord
    , fromDouble
    , posInfinity
    , negInfinity
    , nan
    , roundedTowardZero
    , roundedUp
    , roundedDown
    , roundedToNearest
    ) where

import Data.Tagged
import Data.Ratio
import Data.Word
import Data.Reflection
#if (__GLASGOW_HASKELL >= 610) && (__GLASGOW_HASKELL__ < 612)
import GHC.Integer.Internals
#elif (__GLASGOW_HASKELL__ >= 612)
import GHC.Integer.GMP.Internals
#endif
import GHC.Exts (Int(..)) 
import Foreign.C.Types
import Data.Number.MPFR (RoundMode(..), Precision, MPFR)
import qualified Data.Number.MPFR as M

newtype Fixed r p = Fixed MPFR deriving (Eq,Show,Ord)

data Near
data Zero
data Up
data Down

instance Reifies Near RoundMode where
    reflect = Tagged Near

instance Reifies Zero RoundMode where
    reflect = Tagged Zero

instance Reifies Up RoundMode where
    reflect = Tagged Up

instance Reifies Down RoundMode where
    reflect = Tagged Down

instance Reifies Float Precision where
    reflect = floatPrecision

instance Reifies CFloat Precision where
    reflect = floatPrecision

instance Reifies Double Precision where
    reflect = floatPrecision

instance Reifies CDouble Precision where
    reflect = floatPrecision

floatPrecision :: RealFloat a => Tagged a Precision
floatPrecision = r
    where 
        r = Tagged (fromIntegral (floatDigits (undefined `asArg1Of` r)))
        asArg1Of :: a -> f a b -> a 
        asArg1Of = const

untagMode :: Tagged r a -> Fixed r p -> a
untagMode (Tagged t) _ = t

untagPrecision :: Tagged p a -> Fixed r p -> a 
untagPrecision (Tagged t) _ = t

reflectMode :: Reifies r RoundMode => Fixed r p -> RoundMode
reflectMode = untagMode reflect

reflectPrecision :: Reifies p Precision => Fixed r p -> Precision
reflectPrecision = untagPrecision reflect

liftFrom :: 
    ( Reifies r RoundMode
    , Reifies p Precision
    ) => 
    (RoundMode -> Precision -> a -> MPFR) -> 
    a -> Fixed r p 
liftFrom f a = r where r= Fixed $ f (reflectMode r) (reflectPrecision r) a 

fromMPFR :: (Reifies r RoundMode, Reifies p Precision) => MPFR -> Fixed r p 
fromMPFR = liftFrom M.set

fromInt :: (Reifies r RoundMode, Reifies p Precision) => Int -> Fixed r p 
fromInt = liftFrom M.fromInt

fromWord :: (Reifies r RoundMode, Reifies p Precision) => Word -> Fixed r p 
fromWord = liftFrom M.fromWord

fromDouble :: (Reifies r RoundMode, Reifies p Precision) => Double -> Fixed r p 
fromDouble = liftFrom M.fromDouble

posInfinity :: (Reifies r RoundMode, Reifies p Precision) => Fixed r p
posInfinity = liftFrom (const M.setInf) 1

negInfinity :: (Reifies r RoundMode, Reifies p Precision) => Fixed r p
negInfinity = liftFrom (const M.setInf) (-1)

nan :: (Reifies p Precision) => Fixed r p
nan = r where r = Fixed $ M.setNaN (reflectPrecision r)

lift0 ::
    ( Reifies r RoundMode
    , Reifies p Precision
    ) => 
    (RoundMode -> Precision -> MPFR) -> 
    Fixed r p
lift0 f = r where r = Fixed $ f (reflectMode r) (reflectPrecision r)

lift1 :: 
    ( Reifies r RoundMode
    , Reifies p Precision
    ) => 
    (RoundMode -> Precision -> MPFR -> MPFR) -> 
    Fixed r p -> Fixed r p
lift1 f i@(Fixed a) = Fixed $ f (reflectMode i) (reflectPrecision i) a

lift2 :: 
    ( Reifies r RoundMode
    , Reifies p Precision
    ) => 
    (RoundMode -> Precision -> MPFR -> MPFR -> MPFR) -> 
    Fixed r p -> Fixed r p -> Fixed r p
lift2 f i@(Fixed a) (Fixed b) = Fixed $ f (reflectMode i) (reflectPrecision i) a b

roundedTowardZero :: Reifies p Precision => Fixed Zero p -> Fixed r p
roundedTowardZero (Fixed a) = Fixed a

roundedUp :: Reifies p Precision => Fixed Up p -> Fixed r p
roundedUp (Fixed a) = Fixed a

roundedDown :: Reifies p Precision => Fixed Down p -> Fixed r p
roundedDown (Fixed a) = Fixed a

roundedToNearest :: Reifies p Precision => Fixed Near p -> Fixed r p
roundedToNearest (Fixed a) = Fixed a

instance (Reifies r RoundMode, Reifies p Precision) => Num (Fixed r p) where
    (+)    = lift2 M.add
    (-)    = lift2 M.sub
    (*)    = lift2 M.mul
    negate = lift1 M.neg
    abs    = lift1 M.absD
    signum = undefined -- TODO
    fromInteger (S# i) = fromInt (I# i)
    fromInteger i = roundedTowardZero (liftFrom M.fromIntegerA i)

instance (Reifies r RoundMode, Reifies p Precision) => Real (Fixed r p) where
    toRational (Fixed d) = n % 2 ^ e
        where (n' , e') = M.decompose d
              (n, e) | e' >= 0 = ((n' * 2 ^ e'), 0)
                     | otherwise = (n', - e')

instance (Reifies r RoundMode, Reifies p Precision) => Fractional (Fixed r p) where
    (/) = lift2 M.div
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
    recip d = Fixed M.one / d

instance (Reifies r RoundMode, Reifies p Precision) => Floating (Fixed r p) where
    pi = lift0 M.pi
    exp = lift1 M.exp
    log = lift1 M.log
    sqrt = lift1 M.sqrt
    (**) = lift2 M.pow
    
    sin = lift1 M.sin
    cos = lift1 M.cos
    tan = lift1 M.tan
    asin = lift1 M.asin
    acos = lift1 M.acos
    atan = lift1 M.atan
    sinh = lift1 M.sinh
    cosh = lift1 M.cosh
    tanh = lift1 M.tanh
    asinh = lift1 M.asinh
    acosh = lift1 M.acosh
    atanh = lift1 M.atanh

instance (Reifies r RoundMode, Reifies p Precision) => RealFrac (Fixed r p) where
    properFraction fp@(Fixed d) = (fromIntegral n, Fixed f)
        where r = toRational fp
              m = numerator r
              e = denominator r
              n = quot m e
              f = M.frac Down (M.getPrec d) d

{-
instance (Reifies r RoundMode, Reifies p Precision) => RealFloat (Fixed r p) where
    floatRadix _ = 2
    floatRange _ = (minBound, maxBound)
    floatDigits p = fromIntegral (reflectPrecision p)
    decodeFloat (Fixed d) = (m, fromIntegral e)
        where
            (m,e) = M.decompose d
    -- a whole bunch of other methods
-}
    

-- withPrecision :: Precision -> (forall p. Reifies p Precision => Fixed r p) -> 
