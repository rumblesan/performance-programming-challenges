module Main
  ( main
  ) where

import qualified Data.Bool                     as B
import qualified Data.List                     as L
import           Data.Maybe                     ( fromMaybe, isNothing )
import           System.Environment             ( getArgs )

main :: IO ()
main = do
  args <- parseArgs <$> getArgs
  putStrLn $ show args
  putStrLn $ maybe "no input given" (show . findFactors) args

defaultPrimeCount :: Integer
defaultPrimeCount = 1000

parseArgs :: [String] -> Maybe AppConfig
parseArgs []         = Nothing
parseArgs (num : []) = Just $ AppConfig (read num) defaultPrimeCount
parseArgs (num : primeCount : _) =
  Just $ AppConfig (read num) (read primeCount)

data AppConfig = AppConfig
  { number            :: Integer
  , initialPrimeCount :: Integer
  }
  deriving Show

showOutput :: (Integer, Integer) -> IO ()
showOutput (f1, f2) = putStrLn ("[" ++ show f1 ++ "," ++ show f2 ++ "]")

isqrt :: Integer -> Integer
isqrt n = floor (sqrt $ fromIntegral n :: Double)

findFactors :: AppConfig -> (Integer, Integer)
findFactors cfg =
  let
    maxVal        = isqrt $ number cfg
    primeCount    = min maxVal $ initialPrimeCount cfg
    initialPrimes = L.take (fromIntegral primeCount) $ L.filter isPrime (2:[3,5..]
    seived        = seive initialPrimes (2:[3,5 .. maxVal])
    f1            = fromMaybe 1 $ L.find (isFactor $ number cfg) seived
    f2            = (number cfg) `div` f1
  in
    (f2, f1)

isPrime :: Integer -> Bool
isPrime num = if num > 1
  then L.null [ v | v <- [2 .. isqrt num], f v ]
  else False
  where f = isFactor num

isFactor :: Integer -> Integer -> Bool
isFactor num v = num `mod` v == 0

seive :: [Integer] -> [Integer] -> [Integer]
seive (p : rest) factors =
  p : (seive rest $ L.filter (B.not . isFactor p) factors)
seive [] factors = factors

recseive :: Integer -> Integer -> [Maybe Integer] -> [Maybe Integer]
recseive factor c (x:xs)
  let count = if c + 1 == factor then 0 else c + 1
  in


recseive _ _ [] = []
