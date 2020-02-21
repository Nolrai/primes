module Main (main) where

import Primes (realSieve)


main :: IO ()
main = print $ realSieve (10 ^ (2 :: Int) :: Int)
