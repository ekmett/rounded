{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Numeric.Rounded
import Data.Proxy
main = do
  print (exp pi :: Rounded TowardZero 512)
  print (pi :: Rounded TowardZero Double)
  print (pi :: Rounded AwayFromZero Double)
  print (kCatalan :: Rounded TowardZero 128)
  putStrLn (reifyPrecision 512 (\(_ :: Proxy p) -> show (logBase 10 2 :: Rounded TowardNearest p)))
  putStrLn (reifyRounding TowardZero (\(_ :: Proxy r) -> show (logBase 10 2 :: Rounded r 512)))
  print (fromDouble pi - pi :: Rounded TowardNearest 64)
  print (fromInt 100000000 :: Rounded TowardNearest Float)
  print (fromInt 123456789 :: Rounded TowardNearest Float)
  print (realToFrac (pi :: Rounded TowardNearest 512) :: Double)
