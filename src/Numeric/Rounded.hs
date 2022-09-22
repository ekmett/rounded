-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Rounded
-- Copyright   :  (C) 2012-2014 Edward Kmett, Daniel Peebles
--                (C) 2013-2019 Claude Heiland-Allen
-- License     :  BSD3
-- Maintainer  :  Claude Heiland-Allen <claude@mathr.co.uk>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Numeric.Rounded
    (
    -- * Floating point numbers with a specified rounding mode and precision
      Rounded()
    , fromInt
    , fromDouble
    , fromLongDouble
    , toDouble
    , toLongDouble
    , toInteger'
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
    -- ** Unary
    , abs_
    , acos_
    , acosh_
    , ai_
    , asin_
    , asinh_
    , atan_
    , atanh_
    , cbrt_
    , cos_
    , cosh_
    , cot_
    , coth_
    , csc_
    , csch_
    , digamma_
    , eint_
    , erf_
    , erfc_
    , exp_
    , exp10_
    , exp2_
    , expm1_
    , frac_
    , gamma_
    , j0_
    , j1_
    , li2_
    , lngamma_
    , log_
    , log10_
    , log1p_
    , log2_
    , neg_
    , rec_sqrt_
    , rint_
    , rint_ceil_
    , rint_floor_
    , rint_round_
    , rint_roundeven_
    , rint_trunc_
    , sec_
    , sech_
    , set_
    , sin_
    , sinh_
    , sqr_
    , sqrt_
    , tan_
    , tanh_
    , y0_
    , y1_
    , zeta_
    -- ** Binary
    , add_
    , agm_
    , atan2_
    , copysign_
    , dim_
    , div_
    , fmod_
    , hypot_
    , max_
    , min_
    , mul_
    , pow_
    , sub_
    , beta_
    , gamma_inc_
    -- ** Binary with Int argument
    , jn_
    , yn_
    -- ** Dual output
    , modf
    , sin_cos
    , sinh_cosh
    -- ** Aliases
    , (!+!)
    , (!-!)
    , (!*!)
    , (!/!)
    , (!**!)
    , negate_
    , truncate_
    , ceiling_
    -- ** Comparisons
    , (!==!)
    , (!/=!)
    , (!>=!)
    , (!<=!)
    , (!>!)
    , (!<!)
    , (!<>!)
    -- * Foreign Function Interface
    , withInRounded
    , withInOutRounded
    , withInOutRounded_
    , withOutRounded
    , withOutRounded_
    , unsafeWithOutRounded
    , unsafeWithOutRounded_
    , peekRounded
    ) where

import Numeric.Rounded.Internal
import Numeric.Rounded.Precision
import Numeric.Rounded.Rounding
