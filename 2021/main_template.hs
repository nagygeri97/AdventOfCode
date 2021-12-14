module Main where

import Data.List
import Data.Function
import System.IO

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

convert :: String -> ParsedLine
convert = undefined

solve1 :: [ParsedLine] -> Integer
solve1 = undefined

solve2 :: [ParsedLine] -> Integer
solve2 = undefined