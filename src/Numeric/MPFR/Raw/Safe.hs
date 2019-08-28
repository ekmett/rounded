{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.MPFR.Raw.Safe
-- Copyright   :  (C) 2012-2014 Edward Kmett, Daniel Peebles
--                (C) 2013-2019 Claude Heiland-Allen
-- License     :  BSD3
-- Maintainer  :  Claude Heiland-Allen <claude@mathr.co.uk>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module contains FFI imports as safe ccalls.
----------------------------------------------------------------------------
module Numeric.MPFR.Raw.Safe where

import Foreign.C (CInt(..), CIntMax(..), CSize(..), CChar(..), CLong(..))

import GHC.Exts (Ptr)

import Numeric.LongDouble (LongDouble)
import Numeric.GMP.Types (MPZ, MPQ)

import Numeric.MPFR.Types

foreign import ccall safe "mpfr_init2" mpfr_init2 :: Ptr MPFR -> MPFRPrec -> IO ()
foreign import ccall safe "mpfr_clear" mpfr_clear :: Ptr MPFR -> IO ()

foreign import ccall safe "mpfr_sgn" mpfr_sgn :: Ptr MPFR -> IO CInt
foreign import ccall safe "mpfr_get_d" mpfr_get_d :: Ptr MPFR -> MPFRRnd -> IO Double
foreign import ccall safe "wrapped_mpfr_get_z" wrapped_mpfr_get_z :: Ptr MPZ -> Ptr MPFR -> MPFRRnd -> Ptr CInt -> IO CInt

foreign import ccall safe "mpfr_get_str" mpfr_get_str :: Ptr CChar -> Ptr MPFRExp -> Int -> CSize -> Ptr MPFR -> MPFRRnd -> IO (Ptr CChar)
foreign import ccall safe "mpfr_free_str" mpfr_free_str :: Ptr CChar -> IO ()

foreign import ccall safe "mpfr_set_z" mpfr_set_z :: Ptr MPFR -> Ptr MPZ -> MPFRRnd -> IO CInt
foreign import ccall safe "__gmpfr_set_sj" mpfr_set_sj :: Ptr MPFR -> CIntMax -> MPFRRnd -> IO CInt
foreign import ccall safe "mpfr_set_q" mpfr_set_q :: Ptr MPFR -> Ptr MPQ -> MPFRRnd -> IO CInt
foreign import ccall safe "mpfr_set_d" mpfr_set_d :: Ptr MPFR -> Double -> MPFRRnd -> IO CInt

foreign import ccall safe "wrapped_mpfr_get_z_2exp" wrapped_mpfr_get_z_2exp :: Ptr MPZ -> Ptr MPFR -> Ptr CInt -> IO MPFRExp
foreign import ccall safe "mpfr_set_z_2exp" mpfr_set_z_2exp :: Ptr MPFR -> Ptr MPZ -> MPFRExp -> MPFRRnd -> IO CInt

type Test = Ptr MPFR -> IO CInt

foreign import ccall safe "mpfr_inf_p" mpfr_inf_p :: Test
foreign import ccall safe "mpfr_integer_p" mpfr_integer_p :: Test
foreign import ccall safe "mpfr_nan_p" mpfr_nan_p :: Test
foreign import ccall safe "mpfr_number_p" mpfr_number_p :: Test
foreign import ccall safe "mpfr_regular_p" mpfr_regular_p :: Test
foreign import ccall safe "mpfr_signbit" mpfr_signbit :: Test
foreign import ccall safe "mpfr_zero_p" mpfr_zero_p :: Test

type Fits = Ptr MPFR -> MPFRRnd -> IO CInt

foreign import ccall safe "mpfr_fits_intmax_p" mpfr_fits_intmax_p :: Fits
foreign import ccall safe "mpfr_fits_sint_p" mpfr_fits_sint_p :: Fits
foreign import ccall safe "mpfr_fits_slong_p" mpfr_fits_slong_p :: Fits
foreign import ccall safe "mpfr_fits_sshort_p" mpfr_fits_sshort_p :: Fits
foreign import ccall safe "mpfr_fits_uintmax_p" mpfr_fits_uintmax_p :: Fits
foreign import ccall safe "mpfr_fits_uint_p" mpfr_fits_uint_p :: Fits
foreign import ccall safe "mpfr_fits_ulong_p" mpfr_fits_ulong_p :: Fits
foreign import ccall safe "mpfr_fits_ushort_p" mpfr_fits_ushort_p :: Fits

type Constant = Ptr MPFR -> MPFRRnd -> IO CInt

foreign import ccall safe "mpfr_const_pi" mpfr_const_pi :: Constant
foreign import ccall safe "mpfr_const_log2" mpfr_const_log2 :: Constant
foreign import ccall safe "mpfr_const_euler" mpfr_const_euler :: Constant
foreign import ccall safe "mpfr_const_catalan" mpfr_const_catalan :: Constant

type Unary = Ptr MPFR -> Ptr MPFR -> MPFRRnd -> IO CInt

foreign import ccall safe "mpfr_abs" mpfr_abs :: Unary
foreign import ccall safe "mpfr_acos" mpfr_acos :: Unary
foreign import ccall safe "mpfr_acosh" mpfr_acosh :: Unary
foreign import ccall safe "mpfr_ai" mpfr_ai :: Unary
foreign import ccall safe "mpfr_asin" mpfr_asin :: Unary
foreign import ccall safe "mpfr_asinh" mpfr_asinh :: Unary
foreign import ccall safe "mpfr_atan" mpfr_atan :: Unary
foreign import ccall safe "mpfr_atanh" mpfr_atanh :: Unary
foreign import ccall safe "mpfr_cbrt" mpfr_cbrt :: Unary
foreign import ccall safe "mpfr_cos" mpfr_cos :: Unary
foreign import ccall safe "mpfr_cosh" mpfr_cosh :: Unary
foreign import ccall safe "mpfr_cot" mpfr_cot :: Unary
foreign import ccall safe "mpfr_coth" mpfr_coth :: Unary
foreign import ccall safe "mpfr_csc" mpfr_csc :: Unary
foreign import ccall safe "mpfr_csch" mpfr_csch :: Unary
foreign import ccall safe "mpfr_digamma" mpfr_digamma :: Unary
foreign import ccall safe "mpfr_eint" mpfr_eint :: Unary
foreign import ccall safe "mpfr_erf" mpfr_erf :: Unary
foreign import ccall safe "mpfr_erfc" mpfr_erfc :: Unary
foreign import ccall safe "mpfr_exp" mpfr_exp :: Unary
foreign import ccall safe "mpfr_exp10" mpfr_exp10 :: Unary
foreign import ccall safe "mpfr_exp2" mpfr_exp2 :: Unary
foreign import ccall safe "mpfr_expm1" mpfr_expm1 :: Unary
foreign import ccall safe "mpfr_frac" mpfr_frac :: Unary
foreign import ccall safe "mpfr_gamma" mpfr_gamma :: Unary
foreign import ccall safe "mpfr_j0" mpfr_j0 :: Unary
foreign import ccall safe "mpfr_j1" mpfr_j1 :: Unary
foreign import ccall safe "mpfr_li2" mpfr_li2 :: Unary
foreign import ccall safe "mpfr_lngamma" mpfr_lngamma :: Unary
foreign import ccall safe "mpfr_log" mpfr_log :: Unary
foreign import ccall safe "mpfr_log10" mpfr_log10 :: Unary
foreign import ccall safe "mpfr_log1p" mpfr_log1p :: Unary
foreign import ccall safe "mpfr_log2" mpfr_log2 :: Unary
foreign import ccall safe "mpfr_neg" mpfr_neg :: Unary
foreign import ccall safe "mpfr_rec_sqrt" mpfr_rec_sqrt :: Unary
foreign import ccall safe "mpfr_rint" mpfr_rint :: Unary
foreign import ccall safe "mpfr_rint_ceil" mpfr_rint_ceil :: Unary
foreign import ccall safe "mpfr_rint_floor" mpfr_rint_floor :: Unary
foreign import ccall safe "mpfr_rint_round" mpfr_rint_round :: Unary
foreign import ccall safe "mpfr_rint_roundeven" mpfr_rint_roundeven :: Unary
foreign import ccall safe "mpfr_rint_trunc" mpfr_rint_trunc :: Unary
foreign import ccall safe "mpfr_sec" mpfr_sec :: Unary
foreign import ccall safe "mpfr_sech" mpfr_sech :: Unary
foreign import ccall safe "mpfr_set" mpfr_set :: Unary
foreign import ccall safe "mpfr_sin" mpfr_sin :: Unary
foreign import ccall safe "mpfr_sinh" mpfr_sinh :: Unary
foreign import ccall safe "mpfr_sqr" mpfr_sqr :: Unary
foreign import ccall safe "mpfr_sqrt" mpfr_sqrt :: Unary
foreign import ccall safe "mpfr_tan" mpfr_tan :: Unary
foreign import ccall safe "mpfr_tanh" mpfr_tanh :: Unary
foreign import ccall safe "mpfr_y0" mpfr_y0 :: Unary
foreign import ccall safe "mpfr_y1" mpfr_y1 :: Unary
foreign import ccall safe "mpfr_zeta" mpfr_zeta :: Unary

type Unary' = Ptr MPFR -> Ptr MPFR -> IO CInt

foreign import ccall safe "mpfr_ceil" mpfr_ceil :: Unary'
foreign import ccall safe "mpfr_floor" mpfr_floor :: Unary'
foreign import ccall safe "mpfr_trunc" mpfr_trunc :: Unary'

type Binary = Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> MPFRRnd -> IO CInt

foreign import ccall safe "mpfr_add" mpfr_add :: Binary
foreign import ccall safe "mpfr_agm" mpfr_agm :: Binary
foreign import ccall safe "mpfr_atan2" mpfr_atan2 :: Binary
foreign import ccall safe "mpfr_copysign" mpfr_copysign :: Binary
foreign import ccall safe "mpfr_dim" mpfr_dim :: Binary
foreign import ccall safe "mpfr_div" mpfr_div :: Binary
foreign import ccall safe "mpfr_fmod" mpfr_fmod :: Binary
foreign import ccall safe "mpfr_hypot" mpfr_hypot :: Binary
foreign import ccall safe "mpfr_max" mpfr_max :: Binary
foreign import ccall safe "mpfr_min" mpfr_min :: Binary
foreign import ccall safe "mpfr_mul" mpfr_mul :: Binary
foreign import ccall safe "mpfr_pow" mpfr_pow :: Binary
foreign import ccall safe "mpfr_sub" mpfr_sub :: Binary

type DualOutput = Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> MPFRRnd -> IO CInt

foreign import ccall safe "mpfr_modf" mpfr_modf :: DualOutput
foreign import ccall safe "mpfr_sin_cos" mpfr_sin_cos :: DualOutput
foreign import ccall safe "mpfr_sinh_cosh" mpfr_sinh_cosh :: DualOutput

type Comparison = Ptr MPFR -> Ptr MPFR -> IO CInt

foreign import ccall safe "mpfr_cmp" mpfr_cmp :: Comparison
foreign import ccall safe "mpfr_equal_p" mpfr_equal_p :: Comparison
foreign import ccall safe "mpfr_greaterequal_p" mpfr_greaterequal_p :: Comparison
foreign import ccall safe "mpfr_greater_p" mpfr_greater_p :: Comparison
foreign import ccall safe "mpfr_lessequal_p" mpfr_lessequal_p :: Comparison
foreign import ccall safe "mpfr_lessgreater_p" mpfr_lessgreater_p :: Comparison
foreign import ccall safe "mpfr_less_p" mpfr_less_p :: Comparison
foreign import ccall safe "mpfr_unordered_p" mpfr_unordered_p :: Comparison

foreign import ccall safe "mpfr_nextabove" mpfr_nextabove :: Ptr MPFR -> IO ()
foreign import ccall safe "mpfr_nextbelow" mpfr_nextbelow :: Ptr MPFR -> IO ()

foreign import ccall safe "wrapped_mpfr_get_ld" wrapped_mpfr_get_ld :: Ptr LongDouble -> Ptr MPFR -> MPFRRnd -> Ptr CInt -> IO CInt
foreign import ccall safe "wrapped_mpfr_get_ld_2exp" wrapped_mpfr_get_ld_2exp :: Ptr LongDouble -> Ptr CLong -> Ptr MPFR -> MPFRRnd -> Ptr CInt -> IO CInt
foreign import ccall safe "wrapped_mpfr_set_ld" wrapped_mpfr_set_ld :: Ptr MPFR -> Ptr LongDouble -> MPFRRnd -> Ptr CInt -> IO CInt
foreign import ccall safe "wrapped_mpfr_cmp_ld" wrapped_mpfr_cmp_ld :: Ptr MPFR -> Ptr LongDouble -> IO CInt
