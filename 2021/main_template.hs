module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative

type ParsedLine = undefined

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
convert = undefined

solve1 :: [ParsedLine] -> Int
solve1 = undefined

solve2 :: [ParsedLine] -> Int
solve2 = undefined