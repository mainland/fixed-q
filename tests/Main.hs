{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      :  Main
-- Copyright   :  (c) 2020 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Main (
    main
  ) where

import Data.Bits
import Data.Fixed.Q
import Data.Int
import Data.Word
import Test.HUnit (Assertion, (@?=))
import Test.Hspec
import Test.QuickCheck (Property,
                        (===),
                        property)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "UQ (unsigned)" uqspec
    describe "Q (signed)" qspec

uqspec :: Spec
uqspec = do
    it "bitwise and" $ property prop_uq_and
    it "bitwise or" $ property prop_uq_or
    it "bitwise xor" $ property prop_uq_xor
    it "bitwise complement" $ property prop_uq_complement
    it "bit shift" $ property prop_uq_shift
    it "bit rotate" $ property prop_uq_rotate
    it "bit popCount" $ property prop_uq_popCount
    describe "abs" $ do
      it "abs (0 :: UQ 0 3)"      $ abs (0 :: UQ 0 3) @?= 0
      it "abs (0.125 :: UQ 0 3)"  $ abs (0.125 :: UQ 0 3) @?= 0.125
    describe "signum" $ do
      it "signum (0 :: UQ 1 3)"      $ signum (0 :: UQ 1 3) @?= 0
      it "signum (0.125 :: UQ 1 3)"  $ signum (0.125 :: UQ 1 3) @?= 1

qspec :: Spec
qspec = do
    it "bitwise and" $ property prop_q_and
    it "bitwise or" $ property prop_q_or
    it "bitwise xor" $ property prop_q_xor
    it "bitwise complement" $ property prop_q_complement
    it "bit shift" $ property prop_q_shift
    it "bit rotate" $ property prop_q_rotate
    it "bit popCount" $ property prop_q_popCount
    describe "abs" $ do
      it "abs (-0.375 :: Q 0 3)" $ abs (-0.375 :: Q 0 3) @?= 0.375
      it "abs (0 :: Q 0 3)"      $ abs (0 :: Q 0 3) @?= 0
      it "abs (0.125 :: Q 0 3)"  $ abs (0.125 :: Q 0 3) @?= 0.125
    describe "signum" $ do
      it "signum (-0.375 :: Q 1 3)" $ signum (-0.375 :: Q 1 3) @?= -1
      it "signum (0 :: Q 1 3)"      $ signum (0 :: Q 1 3) @?= 0
      it "signum (0.125 :: Q 1 3)"  $ signum (0.125 :: Q 1 3) @?= 1

testBits1 :: (Bits a, Integral a, Bits b, Show b)
          => (Integer -> b)
          -> (forall c . Bits c => c -> c)
          -> a
          -> Property
testBits1 k op x = op x' === k (fromIntegral (op x))
  where
    x' = k (fromIntegral x)

testBits2 :: (Bits a, Integral a, Bits b, Show b)
          => (Integer -> b)
          -> (forall c . Bits c => c -> c -> c)
          -> a
          -> a
          -> Property
testBits2 k op x y = x' `op` y' === k (fromIntegral (x `op` y))
  where
    x' = k (fromIntegral x)
    y' = k (fromIntegral y)

testBitsResult :: (Bits a, Integral a, Bits b, Eq d, Show d)
               => (Integer -> b)
               -> (forall c . Bits c => c -> d)
               -> a
               -> Property
testBitsResult k op x = op x' === op x
  where
    x' = k (fromIntegral x)

testBitsInt :: (Bits a, Integral a, Bits b, Show b)
            => (Integer -> b)
            -> (forall c . Bits c => c -> Int -> c)
            -> a
            -> Int
            -> Property
testBitsInt k op x i = x' `op` i === k (fromIntegral (x `op` i))
  where
    x' = k (fromIntegral x)

prop_uq_and :: Word32 -> Word32 -> Property
prop_uq_and = testBits2 (UQ :: Integer -> UQ 32 0) (.&.)

prop_uq_or :: Word32 -> Word32 -> Property
prop_uq_or = testBits2 (UQ :: Integer -> UQ 32 0) (.|.)

prop_uq_xor :: Word32 -> Word32 -> Property
prop_uq_xor = testBits2 (UQ :: Integer -> UQ 32 0) xor

prop_uq_complement :: Word32 -> Property
prop_uq_complement = testBits1 (UQ :: Integer -> UQ 32 0) complement

prop_uq_shift :: Word32 -> Int -> Property
prop_uq_shift = testBitsInt (UQ :: Integer -> UQ 32 0) shift

prop_uq_rotate :: Word32 -> Int -> Property
prop_uq_rotate = testBitsInt (UQ :: Integer -> UQ 32 0) rotate

prop_uq_popCount :: Word32 -> Property
prop_uq_popCount = testBitsResult (UQ :: Integer -> UQ 32 0) popCount

prop_q_and :: Int32 -> Int32 -> Property
prop_q_and = testBits2 (Q :: Integer -> Q 31 0) (.&.)

prop_q_or :: Int32 -> Int32 -> Property
prop_q_or = testBits2 (Q :: Integer -> Q 31 0) (.|.)

prop_q_xor :: Int32 -> Int32 -> Property
prop_q_xor = testBits2 (Q :: Integer -> Q 31 0) xor

prop_q_complement :: Int32 -> Property
prop_q_complement = testBits1 (Q :: Integer -> Q 31 0) complement

prop_q_shift :: Int32 -> Int -> Property
prop_q_shift = testBitsInt (Q :: Integer -> Q 31 0) shift

prop_q_rotate :: Int32 -> Int -> Property
prop_q_rotate = testBitsInt (Q :: Integer -> Q 31 0) rotate

prop_q_popCount :: Int32 -> Property
prop_q_popCount = testBitsResult (Q :: Integer -> Q 31 0) popCount

{-
return []

runTests :: IO Bool
runTests = $forAllProperties $
  quickCheckWithResult (stdArgs {maxSuccess = 1000})
-}