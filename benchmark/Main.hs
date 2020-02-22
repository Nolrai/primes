{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Gauge.Main
import Gauge.Main.Options
import System.Environment
import Reified (reifiedFunctions, toPreBench, Reified(..))


main :: IO ()
main =
  do
  argv <- getArgs
  let (config, others) = parseWith defaultConfig argv
  putStrLn $ "others: " <> show others
  putStrLn $ "running with config = " <> show config
  defaultMainWith config benchmarks

benchmarks = onReified <$> take 3 reifiedFunctions

onReified :: Reified IO -> Benchmark
onReified (Reified name _description function)
  = bgroup name $
  do
  (scale :: Int) <- [1..3]
  (digit :: Int) <- [1..9]
  let size = (digit * (10 ^ scale)) + 1
  pure . bench (show size) $ toPreBench function size