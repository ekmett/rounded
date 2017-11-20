{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_HADDOCK not-home #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Rounded.Internal
-- Copyright   :  (C) 2012-2014 Edward Kmett, Daniel Peebles
--                (C) 2013-2017 Claude Heiland-Allen
-- License     :  LGPL
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Numeric.Rounded.Internal where

import GHC.Prim ( ByteArray# )

import Numeric.MPFR.Types
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
