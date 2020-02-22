{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Reified where

import Data.Vector.Unboxed as V
import System.Console.Argument
import System.Console.Command
import System.Console.Program
import Gauge.Benchmark

import Primes

data Reified m where
  Reified :: forall a m
    . (ToPreBench a, ToAction m a) =>
    String ->
    String ->
    a ->
    Reified m

class ToAction m a where
  toAction :: a -> Action m

instance MonadIO m => ToAction m Integer where
  toAction x = io $ print x

instance MonadIO m => ToAction m Int where
  toAction x = io $ print x

instance MonadIO m => ToAction m Bool where
  toAction x = io $ print x

instance (MonadIO m, ToAction m i, Show i) => ToAction m [i] where
  toAction x = io $ print x

instance (MonadIO m, ToAction m i, Unbox i, Show i) => ToAction m (Vector i) where
  toAction x = toAction (V.toList x)

instance (MonadIO m, ToAction m a) => ToAction m (Int -> a) where
  toAction f = withNonOption (fromIntegral <$> natural) (toAction . f)

instance (MonadIO m, ToAction m a) => ToAction m (Integer -> a) where
  toAction f = withNonOption natural (toAction . f)

class ToPreBench a where
  toPreBench :: a -> Int -> Benchmarkable

instance ToPreBench (Int -> [Int]) where
  toPreBench a n = nf a n

instance ToPreBench (Int -> Bool) where
  toPreBench a n = nf a n

instance ToPreBench (Int -> Int -> Vector Int) where
  toPreBench a n = nf (uncurry a) (n * n * 10, n)

reifiedFunctions :: MonadIO m => [Reified m]
reifiedFunctions =
  [Reified "naive" "just primality testing" (naive :: Int -> [Int])
  , Reified "fakeSieve" "simple but slow psudo sieve"
    (fakeSieve :: Int -> [Int])
  , Reified "realSieve" "fast but memory hog sieve" realSieve
  , Reified "multiples" "used in realSieve" multiples
  , Reified "isPrime" "is the number prime?" (isPrime :: Int -> Bool)
  ]