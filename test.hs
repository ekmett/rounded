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