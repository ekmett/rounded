{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Numeric.Rounded.Interval where

import Numeric.Rounded
import Data.Coerce
import Data.Typeable
import GHC.Generics

data Interval p
  = I (Rounded TowardNegInf p) (Rounded TowardInf p)
  | Empty
  deriving (Typeable, Generic)

(...) :: Rounded TowardNegInf p -> Rounded TowardInf p -> Interval p
a ... b
  | coerce a <= b = I a b
  | otherwise = Empty

infixl 6 +/-

(+/-) :: Rounded r p -> Rounded r' p -> Interval p
a +/- b = (coerce a .-. coerce b) ... (coerce a .+. coerce b)

negInfinity :: Fractional a => a
negInfinity = (-1)/0
{-# INLINE negInfinity #-}

posInfinity :: Fractional a => a
posInfinity = 1/0
{-# INLINE posInfinity #-}

-- | create a non-empty interval or fail
interval :: Rounded TowardNegInf p -> Rounded TowardInf p -> Maybe (Interval p)
interval a b
  | coerce a <= b = Just $ I a b
  | otherwise     = Nothing
{-# INLINE interval #-}

-- | The whole real number line
--
-- >>> whole
-- -Infinity ... Infinity
whole :: Precision p => Interval p
whole = I negInfinity posInfinity
{-# INLINE whole #-}

-- | An empty interval
--
-- >>> empty
-- Empty
empty :: Interval p
empty = Empty
{-# INLINE empty #-}

-- | Check if an interval is empty
--
-- >>> null (1 ... 5)
-- False
--
-- >>> null (1 ... 1)
-- False
--
-- >>> null empty
-- True
null :: Interval p -> Bool
null Empty = True
null _ = False
{-# INLINE null #-}

-- | A singleton point
--
-- >>> singleton 1
-- 1 ... 1
singleton :: Rounded r p -> Interval p
singleton a = I (coerce a) (coerce a)
{-# INLINE singleton #-}

-- | The infimum (lower bound) of an interval
--
-- >>> inf (1.0 ... 20.0)
-- 1.0
--
-- >>> inf empty
-- *** Exception: empty interval
inf :: Interval p -> Rounded TowardNegInf p
inf (I a _) = a
inf Empty = error "empty interval"
{-# INLINE inf #-}

-- | The supremum (upper bound) of an interval
--
-- >>> sup (1.0 ... 20.0)
-- 20.0
--
-- >>> sup empty
-- *** Exception: empty interval
sup :: Interval p -> Rounded TowardInf p
sup (I _ b) = b
sup Empty = error "empty interval"
{-# INLINE sup #-}

-- | Is the interval a singleton point?
-- N.B. This is fairly fragile and likely will not hold after
-- even a few operations that only involve singletons
--
-- >>> singular (singleton 1)
-- True
--
-- >>> singular (1.0 ... 20.0)
-- False
singular :: Interval p -> Bool
singular Empty = False
singular (I a b) = coerce a == b
{-# INLINE singular #-}

instance Eq (Interval p) where
  -- (==) = (==!)
  -- {-# INLINE (==) #-}

instance Precision p => Show (Interval p) where
  showsPrec _ Empty = showString "Empty"
  showsPrec n (I a b) =
    showParen (n > 3) $
      showsPrec 3 a .
      showString " ... " .
      showsPrec 3 b

-- | Calculate the width of an interval.
--
-- >>> width (1 ... 20)
-- 19 ... 19
--
-- >>> width (singleton 1)
-- 0 ... 0
--
-- >>> width empty
-- 0 ... 0
width :: Precision p => Interval p -> Rounded TowardInf p
width (I a b) = b - coerce a
width Empty   = 0
{-# INLINE width #-}

-- | Magnitude
--
-- >>> magnitude (1 ... 20)
-- 20
--
-- >>> magnitude (-20 ... 10)
-- 20
--
-- >>> magnitude (singleton 5)
-- 5
--
-- throws 'EmptyInterval' if the interval is empty.
--
-- >>> magnitude empty
-- *** Exception: empty interval
magnitude :: Precision p => Interval p -> Rounded TowardInf p
magnitude = sup . abs
{-# INLINE magnitude #-}

-- | \"mignitude\"
--
-- >>> mignitude (1 ... 20)
-- 1
--
-- >>> mignitude (-20 ... 10)
-- 0
--
-- >>> mignitude (singleton 5)
-- 5
--
-- throws 'EmptyInterval' if the interval is empty.
--
-- >>> mignitude empty
-- *** Exception: empty interval
mignitude :: Precision p => Interval p -> Rounded TowardNegInf p -- TowardZero?
mignitude = inf . abs
{-# INLINE mignitude #-}

-- | Construct a symmetric interval.
--
-- >>> symmetric 3
-- -3 ... 3
symmetric :: Rounded TowardInf p -> Interval p
symmetric b = coerce (negate' b) ... b

-- | Hausdorff distance between intervals.
--
-- >>> distance (1 ... 7) (6 ... 10)
-- 0
--
-- >>> distance (1 ... 7) (15 ... 24)
-- 8
--
-- >>> distance (1 ... 7) (-10 ... -2)
-- 3
--
-- >>> distance Empty (1 ... 1)
-- *** Exception: empty interval
distance :: Precision p => Interval p -> Interval p -> Rounded TowardNegInf p -- TowardZero?
distance i1 i2 = mignitude (i1 - i2)

-- | Inflate an interval by enlarging it at both ends.
--
-- >>> inflate 3 (-1 ... 7)
-- -4 ... 10
--
-- >>> inflate (-2) (0 ... 4)
-- -2 ... 6
--
-- >>> inflate 1 empty
-- Empty
inflate :: Precision p => Rounded TowardInf p -> Interval p -> Interval p
inflate x y = symmetric x + y

{-

-- | Deflate an interval by shrinking it from both ends.
--
-- >>> deflate 3.0 (-4.0 ... 10.0)
-- -1.0 ... 7.0
--
-- >>> deflate 2.0 (-1.0 ... 1.0)
-- Empty
--
-- >>> deflate 1.0 empty
-- Empty
deflate :: => a -> Interval a -> Interval a
deflate _ Empty               = Empty
deflate x (I a b) | a' <= b'  = I a' b'
                  | otherwise = Empty
  where
    a' = a + x
    b' = b - x

-}

instance Precision p => Num (Interval p) where
  I a b + I a' b' = I (a + a') (b + b')
  _ + _ = Empty
