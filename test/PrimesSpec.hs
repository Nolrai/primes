{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module PrimesSpec
  ( spec,
  )
where

import Data.Vector.Unboxed as V
import Primes
  ( fakeSieve,
    isPrime,
    multiples,
    naive,
    realSieve,
  )
import Test.Hspec
import Test.Hspec.Golden
import Test.QuickCheck

primesLessThen100 :: Num n => [n]
primesLessThen100 =
  [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]

spec :: Spec
spec =
  do
    describe (show 'isPrime)
      $ it "is always false on a composit"
      $ property
      $ \(Positive n) (Positive (m :: Int)) ->
        n >= 2 ==> m >= 2
          ==> isPrime (n * m) `shouldBe` False
    describe (show 'multiples) $ do
      it (show 'multiples <> " end n are all < end") . property $
        \(Positive end) (Positive n) ->
          end > n
            ==> n >= 2
            ==> multiples end n `shouldSatisfy` V.all (< end)
      it (show 'multiples <> " end n are all >= (n*n)") . property $
        \(Positive end) (Positive n) ->
          end > n
            ==> n >= 2
            ==> multiples end n `shouldSatisfy` V.all (>= n * n)
      it (show 'multiples <> " end n are all multiples of n") . property $
        \(Positive end) (Positive n) ->
          end > n
            ==> n >= 2
            ==> multiples end n `shouldSatisfy` V.all (\x -> x `mod` n == 0)
      it (show 'multiples <> " is maximal") . property $
        \(Positive end) (Positive n) (Positive k) ->
          n >= 2
            ==> ((k + n) * n) < (end * 10)
            ==> multiples (end * 10) n `shouldSatisfy` V.elem ((k + n) * n)
    describe (show 'naive)
      $ it "produces the primes less then 100"
      $ naive 100 `shouldBe` primesLessThen100
