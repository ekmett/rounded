# Revision history for rounded

## 1.1.1 -- 2023-10-02

* Add `gmp` to `pkgconfig-depends`.  Fixes: <https://github.com/ekmett/rounded/issues/35>.
* Bump dependency versions.  Fixes: <https://github.com/ekmett/rounded/issues/34>.

## 1.1 -- 2020-05-19

* Fixed `simplify` with `Nat` precisions by adding kind signatures.

## 1.0 -- 2019-08-28

* Require MPFR 4.0 or above.

## 0.2 -- 2019-08-28

* Wrapped a lot more of the MPFR 3.1 API.
* Fixed `wrapped_mpfr_set_ld()` FFI binding.  Previously `fromLongDouble`
  could corrupt memory or crash due to missing argument.
* Removed dependency on `singletons`.

## 0.1.0.1 -- 2018-10-31

* License changed from LGPL to BSD3.

## 0.1 -- 2018-10-30

* Bindings rewritten as a straight FFI binding without a patched `MPFR`.
* Added `Simple` module.

## 0.0.1

* Repository initialized based on older work on a package called `precision`
* Daniel Peebles was able to get a patched `MPFR` to work
