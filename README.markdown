fixed-precision
===============

Provides a light-weight interface to Ales Bisjak's hmpfr bindings to MPFR for Haskell.

Phantom types carry the information about the precision and rounding mode.

Usage
-----

> sin pi :: Fixed Down Double  -- use a 53 bit mantissa, and round down intermediate results
> pi :: Fixed Near $(bits 256) -- use a 256 bit mantissa, and round intermediate results to the nearest value
> withPrecision 256 (exp pi)   -- compute using a precision that is specified at runtime.

Contact Information
-------------------

Please, feel free to contact me with questions, concerns, or bug fixes.

I can be reached via github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
