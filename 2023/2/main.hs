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

data Color = Red | Green | Blue deriving (Eq, Ord, Show)
type Draw = (Color, Int)
type Set = [Draw]
type Game = (Int, [Set])

type ParsedLine = Game

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
convert line = (gameNumber, sets)
    where
        gameNumber = gameNumberFromLine line
        sets = setsFromLine line

gameNumberFromLine :: String -> Int
gameNumberFromLine = read . init . (!!1) . words

removeGame :: String -> String
removeGame = unwords . drop 2 . words

setsFromLine :: String -> [Set]
setsFromLine = map parseSet . splitBy ';' . removeGame

parseSet :: String -> Set
parseSet = map parseDraw . splitBy ','

parseDraw :: String -> Draw
parseDraw text = (parseColor colorStr, read numberStr) 
    where 
        [numberStr, colorStr] = words text

parseColor :: String -> Color
parseColor "red" = Red
parseColor "green" = Green
parseColor "blue" = Blue

convert2 :: String -> ParsedLine2
convert2 = convert

solve1 :: [ParsedLine] -> Int
solve1 = sum . map fst . filter gamePasses

gamePasses :: Game -> Bool
gamePasses = all (all drawPasses) . snd 

drawPasses :: Draw -> Bool
drawPasses (Red, n) = n <= 12
drawPasses (Green, n) = n <= 13
drawPasses (Blue, n) = n <= 14

solve2 :: [ParsedLine2] -> Int
solve2 = sum . map power

power :: Game -> Int
power = product . map snd . nubBy ((==) `on` fst) . sortBy (flip compare `on` snd) . concat . snd