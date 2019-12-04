module Day4 where
import Data.List

digitize :: Integral a => a -> [a]
digitize 0 = []
digitize n = n `mod` 10 : digitize (n `div` 10)

isDecreasing :: Ord a => [a] -> Bool
isDecreasing [] = True
isDecreasing [x] = True
isDecreasing (x:y:xs) 
 | x >= y = isDecreasing (y:xs)
 | otherwise = False

hasRepeats :: Eq a => [a] -> Bool
hasRepeats = not . null . filter ((>1) . length) . group

hasRepeats2 :: Eq a => [a] -> Bool
hasRepeats2 = not . null . filter ((<3) . length) . filter ((>1) . length) . group

isValid :: Integral a => a -> Bool
isValid n = isDecreasing digitized && hasRepeats digitized
  where 
    digitized = (digitize n)

isValid2 :: Integral a => a -> Bool
isValid2 n = isDecreasing digitized && hasRepeats2 digitized
    where 
    digitized = (digitize n)

solve :: Integral a => a -> a -> Int
solve a b = length $ filter isValid [a..b]

solve2 :: Integral a => a -> a -> Int
solve2 a b = length $ filter isValid2 [a..b]