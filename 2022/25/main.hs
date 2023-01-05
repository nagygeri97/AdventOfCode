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
        else do line <- getLine
                mainLoop (xs ++ [convert line]) (ys ++ [convert2 line])

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim list
    | null last = [first]
    | otherwise = first : splitBy delim (tail last)
    where
        (first,last) = break (==delim) list

decimalToSnafu :: Int -> String
decimalToSnafu = base5ToSnafu . decimalToBase5

base5ToSnafu :: String -> String
base5ToSnafu = reverse . base5ToSnafu' 0 . reverse
    where
        base5ToSnafu' 0 [] = []
        base5ToSnafu' 1 [] = "1"
        base5ToSnafu' carry (x:xs) = snafuDigit : base5ToSnafu' nextCarry xs
            where 
                (snafuDigit, nextCarry) = base5DigitToSnafuWithCarry $ chr (ord x + carry)

base5DigitToSnafuWithCarry :: Char -> (Char, Int)
base5DigitToSnafuWithCarry '0' = ('0', 0)
base5DigitToSnafuWithCarry '1' = ('1', 0)
base5DigitToSnafuWithCarry '2' = ('2', 0)
base5DigitToSnafuWithCarry '3' = ('=', 1)
base5DigitToSnafuWithCarry '4' = ('-', 1)
base5DigitToSnafuWithCarry '5' = ('0', 1)

decimalToBase5 :: Int -> String
decimalToBase5 = reverse . decimalToBase5'
    where
        decimalToBase5' 0 = ""
        decimalToBase5' n = chr (ord '0' + n `mod` 5) : decimalToBase5' (n `div` 5) 

snafuDigitToDecimal :: Char -> Int
snafuDigitToDecimal '=' = -2
snafuDigitToDecimal '-' = -1
snafuDigitToDecimal '0' = 0
snafuDigitToDecimal '1' = 1
snafuDigitToDecimal '2' = 2

snafuToDecimal :: String -> Int
snafuToDecimal = snafuToDecimal' . reverse
    where
        snafuToDecimal' [] = 0
        snafuToDecimal' (x:xs) = (snafuDigitToDecimal x) + 5 * snafuToDecimal' xs

convert :: String -> ParsedLine
convert = snafuToDecimal

convert2 :: String -> ParsedLine2
convert2 = convert

solve1 :: [ParsedLine] -> String
solve1 = decimalToSnafu . sum

solve2 :: [ParsedLine2] -> Int
solve2 = undefined