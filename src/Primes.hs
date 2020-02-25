{-# LANGUAGE ScopedTypeVariables #-}

module Primes
  ( isPrime,
    naive,
    fakeSieve,
    realSieve,
    multiples,
  )
where

import Control.Monad as M
import Control.Monad.ST
import Data.List as L
import Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable as VM

isPrime :: Integral t => t -> Bool
isPrime n =
  (n > 1 &&)
    . L.all (\x -> n `mod` x /= 0)
    $ L.takeWhile (\x -> x * x <= n) [2 ..]

naive :: Integral t => t -> [t]
naive upperBound = L.filter isPrime [2 .. (upperBound - 1)]

fakeSieve :: forall t. Integral t => t -> [t]
fakeSieve upperBound = go [2 .. (upperBound - 1)]
  where
    go :: [t] -> [t]
    go (x : xs) = x : go (L.filter (\n -> n `mod` x /= 0) xs)
      where
    go [] = []

realSieve :: HasCallStack => Int -> [Int]
realSieve upperBound =
  runST $ seive upperBound

seive ::
  HasCallStack =>
  forall s. Int -> ST s [Int]
seive upperBound =
  do
    v <- VM.replicate upperBound True
    l <- go v `M.mapM` [2 .. (upperBound -1)]
    pure (L.concat l)
  where
    go v n =
      do
        nIsPrime <- read v n
        if nIsPrime
          then do
            (\w -> write v w False) `V.mapM_` multiples upperBound n
            pure [n]
          else pure []

multiples :: HasCallStack => Int -> Int -> Vector Int
multiples upperBound n =
  iterateN (1 + ((upperBound - 1) - n * n) `div` n) (+ n) (n * n)
