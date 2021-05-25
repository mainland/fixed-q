{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- |
-- Module      :  Data.Fixed.Q
-- Copyright   :  (c) 2020-2021 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines signed and unsigned Q format fixed-point numbers.

module Data.Fixed.Q (
    FixedBits(..),

    UQ(..),
    Q(..),

    isZeroNat,
    isLteNat
  ) where

import Data.Bits
import Data.Proxy ( Proxy(..) )
import Data.Ratio ( (%), denominator, numerator )
import Data.Type.Equality ( type (:~:)(..) )
import GHC.TypeNats ( KnownNat, Nat, type (+), type (<=?), natVal, type (<=) )
#if defined(MPFR)
import Numeric.Rounded
#endif /* defined(MPFR) */
import Test.QuickCheck
    ( Arbitrary(..), arbitrarySizedBoundedIntegral, arbitrarySizedFractional )
import Unsafe.Coerce ( unsafeCoerce )

-- | The 'FixedBits' class denotes fixed-point types with finite bits.
class FiniteBits a => FixedBits a where
    -- | Number of bits use to represent sign of fixed-point number.
    signBitSize :: a -> Int

    -- | Number of bits use to represent integral portion of fixed-point number.
    intBitSize :: a -> Int

    -- | Number of bits use to represent fractional portion of fixed-point number.
    fracBitSize :: a -> Int

    -- | Sign bit
    signBit :: a -> Int

    -- | Unit in the last place
    ulp :: a
    ulp = bit 0

-- | Unsigned Q format fixed-point number with @m@ integer bits and @f@
-- fractional bits.
newtype UQ (m :: Nat) (f :: Nat) = UQ { unUQ :: Integer }
  deriving (Eq, Ord)

mkUQ :: forall m f . (KnownNat m, KnownNat f) => Integer -> UQ m f
mkUQ x = UQ (x `mod` (2 ^ finiteBitSize (undefined :: UQ m f)))

instance (KnownNat m, KnownNat f) => Show (UQ m f) where
    show (UQ x) | fracbits == 0 = show x
#if MPFR
                | otherwise     = show (fromIntegral x / 2 ^ fracbits :: Rounded 'TowardNearest (m+f))
#else /* !defined(MPFR) */
                | otherwise     = show (fromIntegral x / 2 ^ fracbits :: Double)
#endif /* !defined(MPFR) */
      where
        fracbits :: Int
        fracbits = fracBitSize (undefined :: UQ m f)

instance (KnownNat m, KnownNat f) => Bits (UQ m f) where
    UQ x .&.   UQ y = UQ (x .&. y)
    UQ x .|.   UQ y = UQ (x .|. y)
    UQ x `xor` UQ y = UQ (x `xor` y)

    complement x@(UQ i) = mkUQ (complement i + 2^finiteBitSize x)

    zeroBits = UQ 0

    isSigned _ = False

    testBit (UQ x) = testBit x

    bit i = mkUQ (shiftL 1 i)

    shift (UQ x) i = mkUQ (shift x i)

    rotate x i =
        mkUQ $ foldr (.|.) 0 [if testBit x j then 1 `shift` ((i+j) `mod` n) else 0 |
                              j <- [0..n-1]]
      where
        n = finiteBitSize x

    popCount (UQ x) = popCount x

    bitSizeMaybe x = Just $ finiteBitSize x

    bitSize = finiteBitSize

instance (KnownNat m, KnownNat f) => FiniteBits (UQ m f) where
    finiteBitSize x = intBitSize x + fracBitSize x

instance (KnownNat m, KnownNat f) => FixedBits (UQ m f) where
    signBitSize _ = 0

    intBitSize _ = fromIntegral $ natVal (Proxy :: Proxy m)

    fracBitSize _ = fromIntegral $ natVal (Proxy :: Proxy f)

    signBit _ = 0

instance (KnownNat m, KnownNat f) => Enum (UQ m f) where
    fromEnum (UQ x) = fromEnum x
    toEnum n = mkUQ (toInteger n)

instance (KnownNat m, KnownNat f) => Bounded (UQ m f) where
    minBound = UQ 0
    maxBound = UQ (2 ^ finiteBitSize (undefined :: UQ m f) - 1)

instance (KnownNat m, KnownNat f) => Num (UQ m f) where
    UQ x + UQ y = mkUQ (x + y)
    UQ x - UQ y = mkUQ (x - y)
    UQ x * UQ y = mkUQ ((x * y) `shiftR` fracbits)
      where
        fracbits :: Int
        fracbits = fracBitSize (undefined :: UQ m f)

    abs x = x

    signum (UQ 0) = 0
    signum _      = 1

    fromInteger i = mkUQ (i `shift` fracbits)
      where
        fracbits :: Int
        fracbits = fracBitSize (undefined :: UQ m f)

instance (KnownNat m, KnownNat f) => Real (UQ m f) where
    toRational (UQ x) = x % (2 ^ fracbits)
      where
        fracbits :: Int
        fracbits = fracBitSize (undefined :: UQ m f)

instance KnownNat m => Integral (UQ m 0) where
    toInteger (UQ x) = x

    quotRem (UQ x) (UQ y) = (mkUQ q, mkUQ r)
      where
        (q, r) = quotRem x y

instance (KnownNat m, KnownNat f, 1 <= f) => Fractional (UQ m f) where
    fromRational r = mkUQ (numerator r * 2^fracbits `div` denominator r)
      where
        fracbits :: Int
        fracbits = fracBitSize (undefined :: UQ m f)

    UQ x / UQ y = mkUQ ((x * 2^fracbits) `div` y)
      where
        fracbits :: Int
        fracbits = fracBitSize (undefined :: UQ m f)

instance (KnownNat m, KnownNat f, 1 <= f) => RealFrac (UQ m f) where
    properFraction x = (i, x - fromIntegral i)
      where
        i = truncate x

    truncate x = truncate (toRational x)
    round x    = round (toRational x)
    ceiling x  = ceiling (toRational x)
    floor x    = floor (toRational x)

-- | Signed Q format fixed-point number with @m@ integer bits and @f@ fractional
-- bits.
newtype Q (m :: Nat) (f :: Nat) = Q { unQ :: Integer }
  deriving (Eq, Ord)

mkQ :: forall m f . (KnownNat m, KnownNat f) => Integer -> Q m f
mkQ x | even q    = Q r
      | otherwise = Q (r - m)
  where
    (q, r) = divMod x m
    m = 2 ^ finiteBitSize (undefined :: UQ m f)

instance (KnownNat m, KnownNat f) => Show (Q m f) where

    show (Q x) | fracbits == 0 = show x
#if MPFR
               | otherwise     = show (fromIntegral x / 2 ^ fracbits :: Rounded 'TowardNearest (m+f))
#else /* !defined(MPFR) */
               | otherwise     = show (fromIntegral x / 2 ^ fracbits :: Double)
#endif /* !defined(MPFR) */
      where
        fracbits :: Int
        fracbits = fracBitSize (undefined :: Q m f)

instance (KnownNat m, KnownNat f) => Bits (Q m f) where
    Q x .&.   Q y = Q (x .&. y)
    Q x .|.   Q y = Q (x .|. y)
    Q x `xor` Q y = Q (x `xor` y)

    complement (Q x) = mkQ (complement x)

    zeroBits = Q 0

    isSigned _ = True

    testBit (Q x) = testBit x

    bit i = mkQ (shiftL 1 i)

    shift (Q x) i = mkQ (shift x i)

    rotate x i =
        mkQ $ foldr (.|.) 0 [if testBit x j then 1 `shift` ((i+j) `mod` n) else 0 |
                             j <- [0..n-1]]
      where
        n = finiteBitSize x

    popCount q@(Q x)
      | x >= 0    = popCount x
      | otherwise = finiteBitSize q - popCount (-x - 1)

    bitSizeMaybe x = Just $ finiteBitSize x

    bitSize = finiteBitSize

instance (KnownNat m, KnownNat f) => FiniteBits (Q m f) where
    finiteBitSize x = 1 + intBitSize x + fracBitSize x

instance (KnownNat m, KnownNat f) => FixedBits (Q m f) where
    signBitSize _ = 1

    intBitSize _ = fromIntegral $ natVal (Proxy :: Proxy m)

    fracBitSize _ = fromIntegral $ natVal (Proxy :: Proxy f)

    signBit x
      | x >= 0    = 0
      | otherwise = 1

instance (KnownNat m, KnownNat f) => Enum (Q m f) where
    fromEnum (Q x) = fromEnum x
    toEnum n = mkQ (toInteger n)

instance (KnownNat m, KnownNat f) => Bounded (Q m f) where
    minBound = Q (-m)
      where
        m = 2 ^ (finiteBitSize (undefined :: Q m f) - 1)

    maxBound = Q (m - 1)
      where
        m = 2 ^ (finiteBitSize (undefined :: Q m f) - 1)

instance (KnownNat m, KnownNat f) => Num (Q m f) where
    Q x + Q y = mkQ (x + y)
    Q x - Q y = mkQ (x - y)
    Q x * Q y = mkQ ((x * y) `shiftR` fracbits)
      where
        fracbits :: Int
        fracbits = fracBitSize (undefined :: Q m f)

    abs (Q x) = Q (abs x)

    signum (Q x) | x < 0     = -1
                 | x == 0    = 0
                 | otherwise = 1

    fromInteger i = mkQ (i `shift` fracbits)
      where
        fracbits :: Int
        fracbits = fracBitSize (undefined :: Q m f)

instance (KnownNat m, KnownNat f) => Real (Q m f) where
    toRational (Q x) = x % (2 ^ fracbits)
      where
        fracbits :: Int
        fracbits = fracBitSize (undefined :: Q m f)

instance KnownNat m => Integral (Q m 0) where
    toInteger (Q x) = x

    quotRem (Q x) (Q y) = (mkQ q, mkQ r)
      where
        (q, r) = quotRem x y

instance (KnownNat m, KnownNat f, 1 <= f) => Fractional (Q m f) where
#if MPFR
    fromRational r = mkQ (round (fromRational r * 2^fracbits :: Rounded 'TowardNearest (m+f)))
#else /* !defined(MPFR) */
    fromRational r = mkQ (round (fromRational r * 2^fracbits :: Double))
#endif /* !defined(MPFR) */
      where
        fracbits :: Int
        fracbits = fracBitSize (undefined :: Q m f)

    Q x / Q y = mkQ (x * 2^fracbits `div` y)
      where
        fracbits :: Int
        fracbits = fracBitSize (undefined :: Q m f)

instance (KnownNat m, KnownNat f, 1 <= f) => RealFrac (Q m f) where
    properFraction x = (i, x - fromIntegral i) where
        i = truncate x

    truncate x = truncate (toRational x)
    round x    = round (toRational x)
    ceiling x  = ceiling (toRational x)
    floor x    = floor (toRational x)

#if defined(MPFR)
type R = 'TowardNearest

liftUQ0 :: (KnownNat m, KnownNat f, KnownNat (m+f), 1 <= f)
        => Rounded R (m+f)
        -> UQ m f
liftUQ0 x = fromRational (toRational x)

liftUQ1 :: (KnownNat m, KnownNat f, 1 <= f)
        => (Rounded R (m+f) -> Rounded R (m+f))
        -> UQ m f
        -> UQ m f
liftUQ1 f x = fromRational (toRational (f (fromRational (toRational x))))

liftQ0 :: (KnownNat m, KnownNat f, 1 <= f)
       => Rounded R (1+m+f)
       -> Q m f
liftQ0 x = fromRational (toRational x)

liftQ1 :: (KnownNat m, KnownNat f, 1 <= f)
       => (Rounded R (1+m+f) -> Rounded R (1+m+f))
       -> Q m f
       -> Q m f
liftQ1 f x = fromRational (toRational (f (fromRational (toRational x))))
#else /* !defined(MPFR) */
liftUQ0 :: (KnownNat m, KnownNat f, 1 <= f)
        => Double
        -> UQ m f
liftUQ0 x = fromRational (toRational x)

liftUQ1 :: (KnownNat m, KnownNat f, 1 <= f)
        => (Double -> Double)
        -> UQ m f
        -> UQ m f
liftUQ1 f x = fromRational (toRational (f (fromRational (toRational x))))

liftQ0 :: (KnownNat m, KnownNat f, 1 <= f)
       => Double
       -> Q m f
liftQ0 x = fromRational (toRational x)

liftQ1 :: (KnownNat m, KnownNat f, 1 <= f)
       => (Double -> Double)
       -> Q m f
       -> Q m f
liftQ1 f x = fromRational (toRational (f (fromRational (toRational x))))
#endif /* !defined(MPFR) */

instance (KnownNat m, KnownNat f, 1 <= f) => Floating (UQ m f) where
    pi = liftUQ0 pi
    exp = liftUQ1 exp
    log = liftUQ1 log
    sin = liftUQ1 sin
    cos = liftUQ1 cos
    asin = liftUQ1 asin
    acos = liftUQ1 acos
    atan = liftUQ1 atan
    sinh = liftUQ1 sinh
    cosh = liftUQ1 cosh
    asinh = liftUQ1 asinh
    acosh = liftUQ1 acosh
    atanh = liftUQ1 atanh

instance (KnownNat m, KnownNat f, 1 <= f) => Floating (Q m f) where
    pi = liftQ0 pi
    exp = liftQ1 exp
    log = liftQ1 log
    sin = liftQ1 sin
    cos = liftQ1 cos
    asin = liftQ1 asin
    acos = liftQ1 acos
    atan = liftQ1 atan
    sinh = liftQ1 sinh
    cosh = liftQ1 cosh
    asinh = liftQ1 asinh
    acosh = liftQ1 acosh
    atanh = liftQ1 atanh

-- | Return a type-level proof that 'n ~ 0'
isZeroNat :: forall n. KnownNat n => Maybe (n :~: 0)
isZeroNat | n == 0    = Just (unsafeCoerce Refl)
          | otherwise = Nothing
  where
    n = natVal (Proxy :: Proxy n)

-- | Return a type-level proof that 'm <=? n'
isLteNat :: forall m n. (KnownNat m, KnownNat n) => Maybe ((m <=? n) :~: 'True)
isLteNat | m <= n    = Just (unsafeCoerce Refl)
         | otherwise = Nothing
  where
    m = natVal (Proxy :: Proxy m)
    n = natVal (Proxy :: Proxy n)

instance (KnownNat m, KnownNat f) => Arbitrary (Q m f) where
    arbitrary = case isZeroNat @f of
                  Just Refl -> arbitrarySizedBoundedIntegral
                  Nothing   -> case isLteNat @1 @f of
                                 Just Refl -> arbitrarySizedFractional
                                 Nothing   -> error "can't happen"

    shrink (Q i) | i > 0     = [Q (i-1)]
                 | i < 0     = [Q (i+1)]
                 | otherwise = []

instance (KnownNat m, KnownNat f) => Arbitrary (UQ m f) where
    arbitrary = case isZeroNat @f of
                  Just Refl -> arbitrarySizedBoundedIntegral
                  Nothing   -> case isLteNat @1 @f of
                                 Just Refl -> arbitrarySizedFractional
                                 Nothing   -> error "can't happen"

    shrink (UQ i) | i > 0     = [UQ (i-1)]
                  | otherwise = []
