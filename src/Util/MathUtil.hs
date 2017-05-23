module Util.MathUtil
( toBounds
, multipliedBy
) where

toBounds :: Ord a => a -> (a, a) -> a
toBounds number (lowerBound, upperBound)
    | number < lowerBound = lowerBound
    | number > upperBound = upperBound
    | otherwise = number

multipliedBy :: Double -> Int -> Int
fraction `multipliedBy` whole =
    let wholeAsDouble = fromIntegral whole
    in floor $ fraction * wholeAsDouble
