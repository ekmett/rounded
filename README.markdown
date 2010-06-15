fixed-precision
===============

Provides a light-weight interface to Ales Bisjak's hmpfr bindings to MPFR for Haskell.

Phantom types carry the information about the precision and rounding mode.

Usage
-----

Use a 53 bit mantissa (the same size as used by a Double), and round down intermediate results:

    sin pi :: Fixed Down Double

Use a 256 bit mantissa, and round intermediate results to the nearest value:

    pi :: Fixed Near $(bits 256)

Specify a mantissa size at runtime:

    reifyPrecision 512 (\(p::p) -> show (logBase 10 2 :: Fixed Near p)) 

Contact Information
-------------------

Please, feel free to contact me with questions, concerns, or bug fixes.

I can be reached via github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
