module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative

type ParsedLine = [(Int, Bool)]

type ParsedLine2 = [(Int, Int)]

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
convert = map ((\x -> (x, False)) . read . (:[]))

calculateVisibles :: ParsedLine -> ParsedLine
calculateVisibles = tail . map fst . scanl (\(_, m) (h, v) -> ((h, h > m || v), max h m)) ((0, False), -1)

calculateRowsVisible :: [ParsedLine] -> [ParsedLine]
calculateRowsVisible = map (calculateVisibles . reverse . calculateVisibles)

convert2 :: String -> ParsedLine2
convert2 = map ((\x -> (x, 1)) . read . (:[]))

calculateVisiblesRight :: ParsedLine2 -> Int
calculateVisiblesRight [_] = 0
calculateVisiblesRight ((h, _):xs) = 1 + (length . takeWhile ((<h) . fst) $ init xs)

calculateScoresRight :: ParsedLine2 -> ParsedLine2
calculateScoresRight [] = []
calculateScoresRight l@((h, s):xs) = (h, s * calculateVisiblesRight l) : calculateScoresRight xs

calculateScoresRow :: [ParsedLine2] -> [ParsedLine2]
calculateScoresRow = map (calculateScoresRight . reverse . calculateScoresRight)

solve1 :: [ParsedLine] -> Int
solve1 = length . filter snd. concat . calculateRowsVisible . transpose . calculateRowsVisible

solve2 :: [ParsedLine2] -> Int
solve2 = maximum . map snd . concat . calculateScoresRow . transpose . calculateScoresRow