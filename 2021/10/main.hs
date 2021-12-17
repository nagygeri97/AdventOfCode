module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import System.IO

type ParsedLine = String

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
convert = id

solve1 :: [ParsedLine] -> Int
solve1 = sum . map (scoreLine [])

scoreLine :: String -> ParsedLine -> Int
scoreLine _ [] = 0
scoreLine stack (x:xs)
    | x `elem` "([{<" = scoreLine (x:stack) xs
    | x == invert (head stack) = scoreLine (tail stack) xs
    | otherwise = score x


invert :: Char -> Char
invert '(' = ')'
invert '[' = ']'
invert '{' = '}'
invert '<' = '>'

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137


solve2 :: [ParsedLine] -> Int
solve2 ls = scores !! mid
    where
        scores = sort . map fromJust . filter isJust . map (complete []) $ ls
        mid = length scores `div` 2

complete :: String -> ParsedLine -> Maybe Int
complete stack [] = Just $ scoreEnd stack
complete stack (x:xs)
    | x `elem` "([{<" = complete ((invert x):stack) xs
    | x == (head stack) = complete (tail stack) xs
    | otherwise = Nothing

scoreEnd :: String -> Int
scoreEnd = foldl (\acc x -> 5*acc + score2 x) 0

score2 :: Char -> Int
score2 ')' = 1
score2 ']' = 2
score2 '}' = 3
score2 '>' = 4
