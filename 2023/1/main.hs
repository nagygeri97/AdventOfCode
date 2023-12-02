module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Monad.State
import Control.Monad
import Control.Applicative

type ParsedLine = Int

type ParsedLine2 = ParsedLine

main :: IO ()
main = mainLoop [] []

mainLoop :: [ParsedLine] -> [ParsedLine2] -> IO ()
mainLoop xs ys = do
    done <- isEOF
    if done
        then do putStrLn . ("Part One: " ++) . show . solve1 $ xs
                putStrLn . ("Part Two: " ++) . show . solve2 $ ys
        else do line <- getLine
                mainLoop (xs ++ [convert line]) (ys ++ [convert2 line])

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim list
    | null last = [first]
    | otherwise = first : splitBy delim (tail last)
    where
        (first,last) = break (==delim) list

convert :: String -> ParsedLine
convert = tupleToNumber . headLast . filter isDigit

headLast :: String -> (Char, Char)
headLast l@(x:xs) = (x, last l)
headLast [] = ('0', '0')

charToNum :: Char -> Int
charToNum c = ord c - ord '0'

tupleToNumber :: (Char, Char) -> Int
tupleToNumber (first, second) = charToNum first * 10 + charToNum second

simplify :: String -> String
simplify [] = []
simplify xs@(h:t)
    | "one" `isPrefixOf` xs = '1':simplify t
    | "two" `isPrefixOf` xs = '2':simplify t
    | "three" `isPrefixOf` xs = '3':simplify t
    | "four" `isPrefixOf` xs = '4':simplify t
    | "five" `isPrefixOf` xs = '5':simplify t
    | "six" `isPrefixOf` xs = '6':simplify t
    | "seven" `isPrefixOf` xs = '7':simplify t
    | "eight" `isPrefixOf` xs = '8':simplify t
    | "nine" `isPrefixOf` xs = '9':simplify t
    | otherwise = h : simplify t 

convert2 :: String -> ParsedLine2
convert2 = convert . simplify

solve1 :: [ParsedLine] -> Int
solve1 = sum

solve2 :: [ParsedLine2] -> Int
solve2 = sum