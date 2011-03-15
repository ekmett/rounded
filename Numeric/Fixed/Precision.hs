{-# LANGUAGE CPP, EmptyDataDecls, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, TemplateHaskell, Rank2Types #-}
module Numeric.Fixed.Precision
    ( Precision
    , reifyPrecision
    , bits
    , bytes
    ) where

import Control.Applicative
import Data.Tagged
import Data.Reflection
import Foreign.C.Types
import Language.Haskell.TH hiding (reify)

class Precision p where
    precision :: Tagged p Int

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

reifyPrecision :: Int -> (forall p. Precision p => p -> a) -> a
reifyPrecision m f = reifyIntegral m (go f)
    where
        go :: ReifiesNum p => (ReifiedPrecision p -> a) -> Tagged (p) a 
        go g = Tagged (g undefined)
{-# INLINE reifyPrecision #-}

floatPrecision :: RealFloat a => Tagged a Int
floatPrecision = r
    where 
        r = Tagged (fromIntegral (floatDigits (undefined `asArg1Of` r)))
        asArg1Of :: a -> f a b -> a 
        asArg1Of = const
{-# INLINE floatPrecision #-}
