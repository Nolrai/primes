{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main,
  )
where

import Annalize
import Criterion
import Criterion.Main
import Criterion.Main.Options
import Criterion.Types
import qualified Data.List as List
import Options.Applicative
import Reified (Reified (..), reifiedFunctions, toPreBench)
import System.IO (hClose)
import System.IO.Temp

opts :: ParserInfo Mode
opts =
  info
    (parseWith defaultConfig <**> helper)
    ( fullDesc
        <> progDesc "benchmarks for Primes"
        <> header "header"
    )

main :: IO ()
main =
  do
    mode <- execParser opts
    putStrLn $ "running with mode = " <> show mode
    case mode of
      Run config' matchType givens ->
        case jsonFile config' of
          Just path -> runBenchmarks mode path
          Nothing -> withTempFile "." "bench.json" $
            \path handle ->
              do
                let newConfig = config' {jsonFile = Just path}
                let newMode = Run newConfig matchType givens
                hClose handle
                runBenchmarks newMode path
      _ -> runBenchmarks mode "bench.json"

runBenchmarks mode path =
  runMode mode benchmarks >> annalize path

benchmarks :: [Benchmark]
benchmarks = onReified <$> List.take 3 reifiedFunctions

onReified :: Reified IO -> Benchmark
onReified (Reified name _description function) =
  bgroup name $
    do
      (nScale :: Int) <- [10 .. 50] -- in decibels effectively
      let floatingNScale :: Float = (fromIntegral nScale / 10)
      let floatingN = 10 ** floatingNScale
      let n :: Int = ceiling floatingN + 1
      pure . bench (show n) $ toPreBench function n
