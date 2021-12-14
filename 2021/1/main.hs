module Main where

import Data.List
import System.IO

main :: IO ()
main = mainLoop []

solve1 :: [Integer] -> Integer
solve1 [] = 0
solve1 (x:xs) = snd $ foldl (\(prev, count) x -> (x, count + if x > prev then 1 else 0))  (x, 0) xs

solve2 :: [Integer] -> Integer
solve2 = solve1 . map sum . filter ((==3) . length) . map (take 3) . tails

mainLoop :: [Integer] -> IO ()
mainLoop xs = do
    done <- isEOF
    if done
        then do putStrLn . ("Part One: " ++) . show . solve1 $ xs
                putStrLn . ("Part Two: " ++) . show . solve2 $ xs
        else do input <- pure read <*> getLine
                mainLoop (xs ++ [input])