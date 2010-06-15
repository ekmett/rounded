{-# LANGUAGE CPP, ScopedTypeVariables, MagicHash, EmptyDataDecls, FlexibleContexts, MultiParamTypeClasses, TemplateHaskell, UndecidableInstances, Rank2Types #-}
module Numeric.Fixed 
    ( Fixed(..)
    , RoundMode(..)
    , Near, Zero, Up, Down
    , Precision
    , reflectRounding
    , reflectPrecision
    , reifyPrecision
    , reifyRounding
    , bits
    , bytes
    , fromMPFR
    , fromInt
    , fromWord
    , fromDouble
    , posInfinity
    , negInfinity
    , nan
    , fromZero, fromUp, fromDown, fromNear
    , toZero, toUp, toDown, toNear
    ) where

import Control.Applicative
import Data.Tagged
import Data.Ratio
import Data.Word
import Data.List (isInfixOf)
import Data.Reflection
#if (__GLASGOW_HASKELL >= 610) && (__GLASGOW_HASKELL__ < 612)
import GHC.Integer.Internals
#elif (__GLASGOW_HASKELL__ >= 612)
import GHC.Integer.GMP.Internals
#endif
import GHC.Exts (Int(..)) 
import Foreign.C.Types
import Data.Number.MPFR (RoundMode(..), MPFR)
import qualified Data.Number.MPFR as M
import Language.Haskell.TH hiding (reify)

newtype Fixed r p = Fixed MPFR deriving (Eq,Ord)

{-# RULES
"realToFrac/Fixed->Fixed" realToFrac = \(Fixed x) -> Fixed x
  #-}

data Near
data Zero
data Up
data Down

class Rounding r where
    rounding :: Tagged r RoundMode

instance Rounding Near where
    rounding = Tagged Near

instance Rounding Zero where
    rounding = Tagged Zero

instance Rounding Up where
    rounding = Tagged Up

instance Rounding Down where
    rounding = Tagged Down

data ReifiedRounding s

retagReifiedRounding :: Tagged s a -> Tagged (ReifiedRounding s) a
retagReifiedRounding = retag
{-# INLINE retagReifiedRounding #-}

instance Reifies s RoundMode => Rounding (ReifiedRounding s) where
    rounding = retagReifiedRounding reflect

reifyRounding :: RoundMode -> (forall r. Rounding r => Tagged r a) -> a
reifyRounding m t = reify m (retagRounding t)
{-# INLINE reifyRounding #-}

retagRounding :: Tagged (ReifiedRounding s) a -> Tagged s a 
retagRounding = retag
{-# INLINE retagRounding #-}

class Precision p where
    precision :: Tagged p M.Precision

instance Precision Float where
    precision = floatPrecision

instance Precision CFloat where
    precision = floatPrecision

instance Precision Double where
    precision = floatPrecision

instance Precision CDouble where
    precision = floatPrecision

data PrecZero
instance Precision PrecZero where
    precision = Tagged 0

data PrecSucc a

retagSucc :: Tagged n a -> Tagged (PrecSucc n) a
retagSucc = retag

instance Precision n => Precision (PrecSucc n) where
    precision = (1+) <$> retagSucc precision 

data PrecDouble a

retagDouble :: Tagged n a -> Tagged (PrecDouble n) a
retagDouble = retag

instance Precision n => Precision (PrecDouble n) where
    precision = (2*) <$> retagDouble precision 

bits :: Int -> Q Type
bits 0 = conT ''PrecZero
bits n = case divMod n 2 of
        (q,0) -> conT ''PrecDouble `appT` bits q
        (0,1) -> conT ''PrecSucc `appT` conT ''PrecZero
        (q,1) -> conT ''PrecSucc `appT` (conT ''PrecDouble `appT` bits q)
        (_,_) -> error "bits: negative"

bytes :: Int -> Q Type
bytes = bits . (*8)

data ReifiedPrecision s

retagReifiedPrecision :: Tagged s a -> Tagged (ReifiedPrecision s) a
retagReifiedPrecision = retag
{-# INLINE retagReifiedPrecision #-}

instance ReifiesNum s => Precision (ReifiedPrecision s) where
    precision = retagReifiedPrecision reflectNum

reifyPrecision :: Int -> (forall p. Precision p => Tagged p a) -> a
reifyPrecision m t = reifyIntegral m (retagPrecision t)
{-# INLINE reifyPrecision #-}

retagPrecision :: Tagged (ReifiedPrecision s) a -> Tagged s a 
retagPrecision = retag
{-# INLINE retagPrecision #-}

floatPrecision :: RealFloat a => Tagged a M.Precision
floatPrecision = r
    where 
        r = Tagged (fromIntegral (floatDigits (undefined `asArg1Of` r)))
        asArg1Of :: a -> f a b -> a 
        asArg1Of = const
{-# INLINE floatPrecision #-}

untagRounding :: Tagged r a -> Fixed r p -> a
untagRounding (Tagged t) _ = t
{-# INLINE untagRounding #-}

untagPrecision :: Tagged p a -> Fixed r p -> a 
untagPrecision (Tagged t) _ = t
{-# INLINE untagPrecision #-}

instance (Rounding r, Precision p) => Show (Fixed r p) where
    show fp = toStringExp decimals fp
        where decimals = ceiling (logBase 10 2 * fromIntegral (reflectPrecision fp) :: Double)

-- | Output an appropriately rounded string in base 10 in exponential form when appropriate
toStringExp :: Rounding r => 
                      Word -- ^ number of digits
                   -> Fixed r p
                   -> String
toStringExp dec fp@(Fixed d)
    | isInfixOf "NaN" ss = "NaN"
    | isInfixOf "Inf" ss = s ++ "Infinity"
    | M.isZero d = "0"
    | e > 0 = 
        s ++ if Prelude.floor prec <= dec
             then take e ss ++ 
                  let bt = backtrim (drop e ss)
                  in if null bt 
                     then "" 
                     else '.' : bt
             else head ss : '.' :
                  let bt = (backtrim . tail) ss 
                  in (if null bt then "0" else bt) ++ 
                     "e" ++ 
                     show (pred e)
    | otherwise = 
        s ++ (head ss : '.' : 
             (let bt = (backtrim . tail) ss 
              in if null bt then "0" else bt) ++ 
             "e" ++ 
             show (pred e))
    where 
        (str, e') = M.mpfrToString (reflectRounding fp) n 10 d
        e = fromIntegral e'
        n        = max dec 5
        (s, ss) = case head str of
            '-' -> ("-", tail str)
            _   -> ("" , str)
        backtrim = reverse . dropWhile (== '0') . reverse 
        prec = logBase 10 2 * fromIntegral (M.getExp d) :: Double


reflectRounding :: Rounding r => Fixed r p -> RoundMode
reflectRounding = untagRounding rounding
{-# INLINE reflectRounding #-}

reflectPrecision :: Precision p => Fixed r p -> M.Precision
reflectPrecision = untagPrecision precision
{-# INLINE reflectPrecision #-}

liftFrom :: 
    ( Rounding r
    , Precision p
    ) => 
    (RoundMode -> M.Precision -> a -> MPFR) -> 
    a -> Fixed r p 
liftFrom f a = r where r = Fixed $ f (reflectRounding r) (reflectPrecision r) a 
{-# INLINE liftFrom #-}

fromMPFR :: (Rounding r, Precision p) => MPFR -> Fixed r p 
fromMPFR = liftFrom M.set
{-# INLINE fromMPFR #-}

fromInt :: (Rounding r, Precision p) => Int -> Fixed r p 
fromInt = liftFrom M.fromInt
{-# INLINE fromInt #-}

fromWord :: (Rounding r, Precision p) => Word -> Fixed r p 
fromWord = liftFrom M.fromWord
{-# INLINE fromWord #-}

fromDouble :: (Rounding r, Precision p) => Double -> Fixed r p 
fromDouble = liftFrom M.fromDouble
{-# INLINE fromDouble #-}

posInfinity :: (Rounding r, Precision p) => Fixed r p
posInfinity = liftFrom (const M.setInf) 1

negInfinity :: (Rounding r, Precision p) => Fixed r p
negInfinity = liftFrom (const M.setInf) (-1)

nan :: (Precision p) => Fixed r p
nan = r where r = Fixed $ M.setNaN (reflectPrecision r)

lift0 ::
    ( Rounding r
    , Precision p
    ) => 
    (RoundMode -> M.Precision -> MPFR) -> 
    Fixed r p
lift0 f = r where r = Fixed $ f (reflectRounding r) (reflectPrecision r)
{-# INLINE lift0 #-}

lift1 :: 
    ( Rounding r
    , Precision p
    ) => 
    (RoundMode -> M.Precision -> MPFR -> MPFR) -> 
    Fixed r p -> Fixed r p
lift1 f i@(Fixed a) = Fixed $ f (reflectRounding i) (reflectPrecision i) a
{-# INLINE lift1 #-}

lift2 :: 
    ( Rounding r
    , Precision p
    ) => 
    (RoundMode -> M.Precision -> MPFR -> MPFR -> MPFR) -> 
    Fixed r p -> Fixed r p -> Fixed r p
lift2 f i@(Fixed a) (Fixed b) = Fixed $ f (reflectRounding i) (reflectPrecision i) a b
{-# INLINE lift2 #-}

toZero :: Precision p => Fixed r p -> Fixed Zero p
toZero (Fixed a) = Fixed a
{-# INLINE toZero #-}

toUp :: Precision p => Fixed r p -> Fixed Up p
toUp (Fixed a) = Fixed a
{-# INLINE toUp #-}

toDown :: Precision p => Fixed r p -> Fixed Down p
toDown (Fixed a) = Fixed a
{-# INLINE toDown #-}

toNear :: Precision p => Fixed r p -> Fixed Near p
toNear (Fixed a) = Fixed a
{-# INLINE toNear #-}

fromZero :: Precision p => Fixed Zero p -> Fixed r p
fromZero (Fixed a) = Fixed a
{-# INLINE fromZero #-}

fromUp :: Precision p => Fixed Up p -> Fixed r p
fromUp (Fixed a) = Fixed a
{-# INLINE fromUp #-}

fromDown :: Precision p => Fixed Down p -> Fixed r p
fromDown (Fixed a) = Fixed a
{-# INLINE fromDown #-}

fromNear :: Precision p => Fixed Near p -> Fixed r p
fromNear (Fixed a) = Fixed a
{-# INLINE fromNear #-}

instance (Rounding r, Precision p) => Num (Fixed r p) where
    (+)    = lift2 M.add
    (-)    = lift2 M.sub
    (*)    = lift2 M.mul
    negate = lift1 M.neg
    abs    = lift1 M.absD
    signum = undefined -- TODO
    fromInteger (S# i) = fromInt (I# i)
    fromInteger i = fromZero (liftFrom M.fromIntegerA i)

instance (Rounding r, Precision p) => Real (Fixed r p) where
    toRational (Fixed d) = n % 2 ^ e
        where (n' , e') = M.decompose d
              (n, e) | e' >= 0 = ((n' * 2 ^ e'), 0)
                     | otherwise = (n', - e')

instance (Rounding r, Precision p) => Fractional (Fixed r p) where
    (/) = lift2 M.div
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
    recip d = Fixed M.one / d

instance (Rounding r, Precision p) => Floating (Fixed r p) where
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

instance (Rounding r, Precision p) => RealFrac (Fixed r p) where
    properFraction fp@(Fixed d) = (fromIntegral n, Fixed f)
        where r = toRational fp
              m = numerator r
              e = denominator r
              n = quot m e
              f = M.frac Down (M.getPrec d) d

{-
instance (Rounding r, Precision p) => RealFloat (Fixed r p) where
    floatRadix _ = 2
    floatRange _ = (minBound, maxBound)
    floatDigits p = fromIntegral (reflectPrecision p)
    decodeFloat (Fixed d) = (m, fromIntegral e)
        where
            (m,e) = M.decompose d
    -- a whole bunch of other methods
-}
