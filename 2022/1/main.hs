module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative

type ParsedLine = String

main :: IO ()
main = mainLoop []

mainLoop :: [ParsedLine] -> IO ()
mainLoop xs = do
    done <- isEOF
    if done
        then do putStrLn . ("Part One: " ++) . show . solve1 $ xs
                putStrLn . ("Part Two: " ++) . show . solve2 $ xs
        else do input <- convert <$> getLine
                mainLoop (xs ++ [input])

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim list
    | null last = [first]
    | otherwise = first : splitBy delim (tail last)
    where
        (first,last) = break (==delim) list

convert :: String -> ParsedLine
convert = id

processInput :: [ParsedLine] -> [[Int]]
processInput = (map . map $ read) . splitBy []

solve1 :: [ParsedLine] -> Int
solve1 = maximum . map sum . processInput

solve2 :: [ParsedLine] -> Int
solve2 = sum . take 3 . sortBy (flip compare) . map sum . processInput