{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Rounded.Precision
-- Copyright   :  (C) 2012 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Numeric.Rounded.Precision
    ( Precision(..)
    , reifyPrecision
    , bits
    , bytes
    , Bytes
    ) where

import Data.Proxy
import Data.Reflection
import Foreign.C.Types
import GHC.Types
import GHC.TypeLits
import GHC.Prim
import Language.Haskell.TH hiding (reify)

-- | This class is used to specify the number of bits of precision that are maintained in the
-- significand of a properly 'Numeric.Rounded.Rounded' floating point number.
class Precision p where
  precision :: proxy p -> Int
  prec# :: proxy p -> Int#
  prec# p = case precision p of
    I# i# -> i#

floatPrecision :: RealFloat a => p a -> Int
floatPrecision p = fromIntegral (floatDigits (proxyArg p)) where
  proxyArg :: p a -> a
  proxyArg _ = undefined
{-# INLINE floatPrecision #-}

instance Precision Float where
  precision = floatPrecision

instance Precision CFloat where
  precision = floatPrecision

instance Precision Double where
  precision = floatPrecision

instance Precision CDouble where
  precision = floatPrecision

instance KnownNat n => Precision (n :: Nat) where
  precision p = max 2 $ fromInteger (natVal p)

data Bytes (n :: Nat)

instance KnownNat n => Precision (Bytes n) where
  precision _ = max 2 $ 8 * fromInteger (natVal (undefined :: Bytes n))

-- | Specify a number of bits of 'Precision' in the significand.
--
-- @type Huge r = 'Rounded' r $('bits' 512)@
--
-- expands to
--
-- @type Huge r = 'Rounded' r 512@
--
-- using @DataKinds@, but can be more useful if you need to do arithmetic at TH evaluation time.
bits :: Int -> Q Type
bits = litT . numTyLit . toInteger

-- | Specify a number of bytes of 'Precision' in the significand.
--
-- @type Huge r = 'Rounded' r $('bytes' 64)@
bytes :: Int -> Q Type
bytes = bits . (*8)

data ReifiedPrecision (s :: *)

retagReifiedPrecision :: (Proxy s -> a) -> proxy (ReifiedPrecision s) -> a
retagReifiedPrecision f _ = f Proxy
{-# INLINE retagReifiedPrecision #-}

instance Reifies s Int => Precision (ReifiedPrecision s) where
  precision = retagReifiedPrecision reflect

reifyPrecision :: Int -> (forall (p :: *). Precision p => Proxy p -> a) -> a
reifyPrecision m f = reify m (go f) where
  go :: Reifies p Int => (Proxy (ReifiedPrecision p) -> a) -> proxy p -> a
  go g _ = g Proxy
{-# INLINE reifyPrecision #-}
