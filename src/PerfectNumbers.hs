module PerfectNumbers (classify, Classification (..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | aliq < n = Just Deficient
  | aliq == n = Just Perfect
  | otherwise = Just Abundant
  where
    aliq = sum $ filter (\i -> mod n i == 0) [1 .. n - 1]