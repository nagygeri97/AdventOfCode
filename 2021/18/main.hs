module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative

data Number
    = Regular {value :: Int}
    | Pair {left :: Number, right :: Number}

type ParsedLine = [Int]

main :: IO ()
main = mainLoop []

mainLoop :: [ParsedLine] -> IO ()
mainLoop xs = do
    done <- isEOF
    if done
        then do putStrLn . ("Part One: " ++) . show . solve1 $ xs
                putStrLn . ("Part Two: " ++) . show . solve2 $ xs
        else do input <- pure convert <*> getLine
                mainLoop (xs ++ [input])

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim list
    | null last = [first]
    | otherwise = first : splitBy delim (tail last)
    where
        (first,last) = break (==delim) list

convert :: String -> ParsedLine
convert = map (\x -> if x == '[' then (-1) else if x == ']' then (-2) else read [x]) . filter (\x -> not $ x == ',') 

splitFirst :: [Int] -> (Bool, [Int])
splitFirst [] = (False, [])
splitFirst (x:xs)
    | x > 9 = (True, (-1) : (x `div` 2) : ((x `div` 2) + (x `mod` 2)) : (-2): xs)
    | otherwise = (didSplit, x : result)
    where
        (didSplit, result) = splitFirst xs

explodeFirst :: [Int] -> Int -> (Bool, Int, [Int])
explodeFirst [] _ = (False, 0, [])
explodeFirst (x:xs) depth
    | depth == 4 && x == (-1) = (True, x1, 0 : addToFirst x2 d)
    | x < 0 = (exploded, toAdd, x:result)
    | otherwise = (exploded, 0, (x+toAdd):result)
    where
        ([x1,x2,_], d) = splitAt 3 xs
        newDepth = if x == (-1) then depth + 1 else if x == (-2) then depth - 1 else depth
        (exploded, toAdd, result) = explodeFirst xs newDepth

addToFirst :: Int -> [Int] -> [Int]
addToFirst v [] = []
addToFirst v (x:xs)
    | x < 0 = x : addToFirst v xs
    | otherwise = (x + v) : xs

reduce :: [Int] -> [Int]
reduce xs
    | exploded = reduce ex
    | didSplit = reduce sp
    | otherwise = xs
    where
        (exploded, _, ex) = explodeFirst xs 0
        (didSplit, sp) = splitFirst xs

add :: [Int] -> [Int] -> [Int]
add xs ys = reduce $ (-1) : (xs ++ ys ++ [(-2)])

magnitude :: [Int] -> Int
magnitude [x] = x
magnitude xs = 3 * magnitude first + 2 * magnitude second
    where
        (first, second) = split (-1) (init $ tail xs)

split :: Int -> [Int] -> ([Int], [Int])
split _ [] = ([],[])
split depth (x:xs)
    | depth == 0 = ([], x:xs)
    | otherwise = (x:f, s)
    where
        fixDepth = if depth < 0 then 0 else depth
        newDepth = if x == (-1) then fixDepth + 1 else if x == (-2) then fixDepth - 1 else fixDepth
        (f, s) = split newDepth xs

solve1 :: [ParsedLine] -> Int
solve1 = magnitude . foldl1 add

choose :: Int -> [a] -> [[a]]
choose 0 _  = [[]]
choose _ [] = []
choose k (x:xs) = [x:c | c <- choose (k-1) xs] ++ choose k xs

solve2 :: [ParsedLine] -> Int
solve2 nums = maximum . map solve1 $ allPairs
    where
        pairs = choose 2 nums
        allPairs = pairs ++ map reverse pairs