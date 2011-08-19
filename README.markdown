precision
===============

Provides a light-weight interface to MPFR for Haskell

Phantom types carry the information about the precision and rounding mode.

Usage
-----

Use a 53 bit mantissa (the same size as used by a Double), and round down intermediate results:

    sin pi :: Rounded TowardZero Double

Use a 256 bit mantissa, and round intermediate results to the nearest value:

    pi :: Rounded TowardNearest $(bits 256)

Specify a mantissa size at runtime:

    reifyPrecision 512 (\(p::p) -> show (logBase 10 2 :: Rounded TowardNearest p)) 

Contact Information
-------------------

Please, feel free to contact me with questions, concerns, or bug fixes.

I can be reached as ekmett via github or as edwardk on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
