module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import System.IO

type ParsedLine = ([String], [String])

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
convert s = (a,b)
    where [a,b] = map words . splitBy '|' $ s

solve1 :: [ParsedLine] -> Int
solve1 = length . filter ((`elem` [2,4,3,7]) . length) . concat . map snd

solve2 :: [ParsedLine] -> Int
solve2 = sum . map solveLine

solveLine :: ParsedLine -> Int
solveLine (patterns, output) = foldl (\acc x -> 10*acc + x) 0 $ map (\x -> fromJust $ elemIndex (sort x) numbers) output
    where
        sorted = sortBy (compare `on` length) patterns

        one = sorted !! 0
        four = sorted !! 2
        seven = sorted !! 1
        eight = sorted !! 9

        fives = take 3 . drop 3 $ sorted
        sixes = take 3 . drop 6 $ sorted

        top = seven \\ one

        bottomLeft = (eight \\ top) \\ four

        two = head $ filter (contains bottomLeft) fives
        three = head $ filter (contains one) fives
        five = head $ fives \\ [two, three]
        six = head $ filter (not . contains one) sixes
        zero = head $ filter (contains bottomLeft) (sixes \\ [six])
        nine = head $ sixes \\ [zero, six]

        numbers = map sort [zero, one, two, three, four, five, six, seven, eight, nine]

contains :: String -> String -> Bool
contains [] s = True
contains (x:xs) s
    | x `elem` s = contains xs s
    | otherwise = False 