module Utils.MathUtil
( toBounds
) where

toBounds :: Ord a => a -> (a, a) -> a
toBounds number (lowerBound, upperBound)
    | number < lowerBound = lowerBound
    | number > upperBound = upperBound
    | otherwise = number
