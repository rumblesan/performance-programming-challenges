module Main (main) where

import System.Environment (getArgs)
import qualified Data.List as L

main :: IO ()
main = do
  args <- getArgs
  case args of
    num:_ -> do
      showOutput $ findFactors $ read num
    _ -> putStrLn "no input given"

showOutput :: (Integer, Integer) -> IO ()
showOutput (f1, f2) = putStrLn ("[" ++ show f1 ++ "," ++ show f2 ++ "]")

findFactors :: Integer -> (Integer, Integer)
findFactors num = 
  let
    maxVal = floor $ (sqrt $ fromIntegral num :: Double)
    initialPrimes = L.filter turnerIsPrime [2..1000]
    seived = seive initialPrimes [2..maxVal]
    f1 = search (isFactor num) seived
    f2 = num `div` f1
  in
    (f2, f1)

isFactor :: Integer -> Integer -> (Integer, Bool)
isFactor num v = (v, num `mod` v == 0)

search :: [Integer -> Bool] -> [(Integer, Bool)] -> Integer
search ((f, check):xs) = if check then f else search xs
search [] = 1

seive :: [Integer] -> [Integer] -> [Integer]
seive [] factors = factors
seive (p:rest) factors = p:(seive rest $ L.filter (\f -> f `mod` p /= 0))
