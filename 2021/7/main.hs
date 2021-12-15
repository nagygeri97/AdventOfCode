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

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim list
    | null last = [first]
    | otherwise = first : splitBy delim (tail last)
    where
        (first,last) = break (==delim) list

convert :: String -> ParsedLine
convert = map read . splitBy ','

solve1 :: ParsedLine -> Int
solve1 xs = minimum $ getMinimal (sort xs) m 0 (length xs) (sum $ map (+(-m)) xs)
    where
        m = minimum xs

getMinimal :: [Int] -> Int -> Int -> Int -> Int -> [Int] 
getMinimal xs current smallers largers cost
    | largers == 0 = [cost]
    | otherwise = cost : getMinimal d (current + 1) ns nl (cost + ns - nl)
    where
        (t,d) = span (==current) xs
        lt = length t
        ns = smallers + lt
        nl = largers - lt

solve2 :: ParsedLine -> Int
solve2 xs = minimum $ getMinimal2 (minimum xs) (maximum xs) xs

getMinimal2 :: Int -> Int -> [Int] -> [Int]
getMinimal2 current end xs
    | current > end = []
    | otherwise = (sum . map (\x -> (x+1) * x `div` 2) . map (abs . (current-)) $ xs) : getMinimal2 (current + 1) end xs