module Main where

import Data.List
import Data.Function
import System.IO

data Cell
    = Marked
    | Unmarked Integer
    deriving (Show, Eq)

type Board = [[Cell]]
type Draws = [Integer]

size :: Integer
size = 5

main :: IO ()
main = do
    draws <- pure parseDraws <*> getLine
    mainLoop draws []

mainLoop :: Draws -> [Board] -> IO ()
mainLoop draws xs = do
    done <- isEOF
    if done
        then do putStrLn . ("Part One: " ++) . show $ solve1 draws xs
                putStrLn . ("Part Two: " ++) . show $ solve2 draws xs
        else do input <- readBoard size
                mainLoop draws (xs ++ [input])

parseDraws :: String -> Draws
parseDraws = map read . words . map (\x -> if x == ',' then ' ' else x)

parseRow :: String -> [Cell]
parseRow = map (Unmarked . read) . words

readBoard :: Integer -> IO Board
readBoard 0 = return []
readBoard n = do
    row <- pure parseRow <*> getLine
    if null row 
        then readBoard n
        else do board <- readBoard (n-1)
                return (row:board)

solve1 :: Draws -> [Board] -> Integer
solve1 (draw:draws) boards
    | null wons = solve1 draws newBoards
    | otherwise = score draw (head wons) 
    where 
        newBoards = (map . map . map) (\x -> if x == Unmarked draw then Marked else x) boards
        wons = filter isWon newBoards

isWon :: Board -> Bool
isWon board = any (all (==Marked)) (board ++ transpose board)

score :: Integer -> Board -> Integer
score draw board = draw * (sum . map fromUnmarked . filter isUnmarked . concat $ board)

isUnmarked :: Cell -> Bool
isUnmarked (Unmarked _) = True
isUnmarked _ = False

fromUnmarked :: Cell -> Integer
fromUnmarked (Unmarked n) = n

solve2 :: Draws -> [Board] -> Integer
solve2 (draw:draws) boards
    | length wons == length boards = score draw (head wons)
    | otherwise = solve2 draws (filter (not . isWon) newBoards) 
    where 
        newBoards = (map . map . map) (\x -> if x == Unmarked draw then Marked else x) boards
        wons = filter isWon newBoards