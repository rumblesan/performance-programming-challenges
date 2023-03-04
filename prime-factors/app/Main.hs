module Main
  ( main
  ) where

import qualified Data.Bool                     as B
import qualified Data.List                     as L
import           Data.Maybe                     ( fromMaybe )
import           System.Environment             ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  case args of
    num : _ -> do
      showOutput $ findFactors $ read num
    _ -> putStrLn "no input given"

showOutput :: (Integer, Integer) -> IO ()
showOutput (f1, f2) = putStrLn ("[" ++ show f1 ++ "," ++ show f2 ++ "]")

initialPrimeCount :: Integer
initialPrimeCount = 1000

isqrt :: Integer -> Integer
isqrt n = floor (sqrt $ fromIntegral n :: Double)

findFactors :: Integer -> (Integer, Integer)
findFactors num =
  let maxVal           = isqrt num
      maxInitialPrimes = max maxVal initialPrimeCount
      initialPrimes    = L.filter isPrime [2 .. maxInitialPrimes]
      seived           = seive initialPrimes [2 .. maxVal]
      f1               = fromMaybe 1 $ L.find (isFactor num) seived
      f2               = num `div` f1
  in  (f2, f1)

isPrime :: Integer -> Bool
isPrime num = if num > 1
  then L.null [ v | v <- [2 .. isqrt num], isFactor num v ]
  else False

isFactor :: Integer -> Integer -> Bool
isFactor num v = num `mod` v == 0

seive :: [Integer] -> [Integer] -> [Integer]
seive (p : rest) factors =
  p : (seive rest $ L.filter (B.not . isFactor p) factors)
seive [] factors = factors
