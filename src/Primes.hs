{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module Primes
       ( isPrime
       , naive
       , fakeSieve
       , realSieve
       , multiples
       ) where

import Data.List as L
import Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable as VM
import Control.Monad.ST
import Control.Monad as M

isPrime :: Integral t => t -> Bool
isPrime n =
  (n > 1 &&)
    . L.all (\x -> n `mod` x /= 0)
    $ L.takeWhile (\x -> x * x <= n) [2 ..]

naive :: (HasCallStack, Integral t) => t -> [t]
naive upperBound = L.filter isPrime [2 .. (upperBound - 1)]

fakeSieve :: forall t. (HasCallStack, Integral t) => t -> [t]
fakeSieve upperBound = go [2.. (upperBound - 1)]
  where
  go :: [t] -> [t]
  go (x:xs) = upToSquare L.++ L.filter (\n -> n `mod` x == 0) (go rest)
    where
    (upToSquare, rest) = L.span (<x*x) xs
  go [] = []

realSieve :: HasCallStack => Int -> [Int]
realSieve upperBound =
  runST $ seive upperBound

seive :: HasCallStack =>
  forall s. Int -> ST s [Int]
seive upperBound =
  do
  v <- VM.replicate upperBound True
  l <- go v `M.mapM` [2 .. (upperBound-1)]
  pure (L.concat l)
  where
  go v n =
    do
    nIsPrime <- read v n
    if nIsPrime
      then do
        (\w -> write v w False) `V.mapM_` multiples upperBound n
        pure [n]
      else
        pure []

multiples :: HasCallStack => Int -> Int -> Vector Int
multiples upperBound n =
  iterateN (1 + ((upperBound - 1) - n*n) `div` n) (+n) (n*n)