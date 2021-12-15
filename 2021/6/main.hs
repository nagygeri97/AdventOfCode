module Main where

import Data.List
import Data.Function
import System.IO

type ParsedLine = [Int]

main :: IO ()
main = mainLoop []

mainLoop :: [ParsedLine] -> IO ()
mainLoop xs = do
    done <- isEOF
    if done
        then do putStrLn . ("Part One: " ++) . show . solve1 $ head xs
                putStrLn . ("Part Two: " ++) . show . solve2 $ head xs
        else do input <- pure convert <*> getLine
                mainLoop (xs ++ [input])

convert :: String -> ParsedLine
convert = map read . words . map (\x -> if x == ',' then ' ' else x)

singleStep :: [Int] -> [Int]
singleStep (x:xs) = t ++ (d+x):ds
    where
        newXs = xs ++ [x]
        (t,d:ds) = splitAt 6 newXs

getWithDefault :: [(Int, Int)] -> Int -> Int -> Int
getWithDefault [] v _ = v
getWithDefault ((a,b):xs) v k
 | a == k = b
 | otherwise = getWithDefault xs v k

solve :: Int -> ParsedLine -> Int
solve n xs = sum $ iterate singleStep start !! n
    where
        start = map (getWithDefault (map (\x -> (head x, length x)) . group . sort $ xs) 0) [0..8]

solve1 :: ParsedLine -> Int
solve1 = solve 80

solve2 :: ParsedLine -> Int
solve2 = solve 256