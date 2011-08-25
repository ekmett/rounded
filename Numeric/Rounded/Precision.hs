{-# LANGUAGE CPP, EmptyDataDecls, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances, TemplateHaskell, Rank2Types, MagicHash #-}
module Numeric.Rounded.Precision
    ( Precision(..)
    , reifyPrecision
    , bits
    , bytes
    ) where

import Control.Applicative
import Data.Reflection
import Foreign.C.Types
import Language.Haskell.TH hiding (reify)
import GHC.Types
import GHC.Prim
import Data.Proxy

class Precision p where
    precision :: proxy p -> Int
    prec# :: proxy p -> Int# 
    prec# p = case precision p of
      I# i# -> i#

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
    precision _ = 0

data PrecSucc a

retagSucc :: (Proxy n -> a) -> proxy (PrecSucc n) -> a
retagSucc f _ = f Proxy

instance Precision n => Precision (PrecSucc n) where
    precision = (1+) <$> retagSucc precision 

data PrecDouble a

retagDouble :: (Proxy n -> a) -> proxy (PrecDouble n) -> a
retagDouble f _ = f Proxy

instance Precision n => Precision (PrecDouble n) where
    precision = (2*) <$> retagDouble precision 

-- | 
-- A Precision for a specified number of bits.
--
-- > type Huge r = Rounded r $(bits 512)
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

retagReifiedPrecision :: (Proxy s -> a) -> proxy (ReifiedPrecision s) -> a
retagReifiedPrecision f _ = f Proxy
{-# INLINE retagReifiedPrecision #-}

instance ReifiesNum s => Precision (ReifiedPrecision s) where
    precision = retagReifiedPrecision reflectNum

reifyPrecision :: Int -> (forall p. Precision p => Proxy p -> a) -> a
reifyPrecision m f = reifyIntegral m (go f)
  where
    go :: ReifiesNum p => (Proxy (ReifiedPrecision p) -> a) -> proxy p -> a 
    go g _ = g Proxy
{-# INLINE reifyPrecision #-}

floatPrecision :: RealFloat a => proxy a -> Int
floatPrecision p = fromIntegral (floatDigits (proxyArg p))
  where 
    proxyArg :: p a -> a 
    proxyArg _ = undefined
{-# INLINE floatPrecision #-}
