module PerfectNumbers (classify, Classification (..)) where

import Data.Foldable (foldl')

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | n == 1 = Just Deficient
  | otherwise =
      let sumDivisors = foldl' (\s i -> s + if n `mod` i == 0 then i + if i /= 1 && i /= n `div` i then n `div` i else 0 else 0) 0 [1 .. floor (sqrt (fromIntegral n :: Double))]
       in Just $ case compare sumDivisors n of
            EQ -> Perfect
            GT -> Abundant
            LT -> Deficient