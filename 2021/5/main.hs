module Main where

import Data.List
import Data.Function
import System.IO

type ParsedLine = ((Int, Int), (Int, Int))

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
convert s = ((x1,y1), (x2,y2))
    where
        [[x1,y1],[x2,y2]] = (map . map) read . map words $ [first, second]
        [first,_,second] = (map . map) (\x -> if x == ',' then ' ' else x) . words $ s

size :: Int
size = 1000

emptyBoard :: [[Int]]
emptyBoard = replicate size . replicate size $ 0

applyLineOnBoard :: ParsedLine -> [[Int]] -> [[Int]]
applyLineOnBoard ((x1,y1), (x2,y2)) board
 | x1 == x2 = incrementAtBoard board x1 (min y1 y2, max y1 y2)
 | y1 == y2 = transpose $ incrementAtBoard (transpose board) y1 (min x1 x2, max x1 x2)
 | otherwise = board

incrementAtBoard :: [[Int]] -> Int -> (Int, Int) -> [[Int]]
incrementAtBoard [] _ _ = []
incrementAtBoard (r:rs) 0 (y1,y2) = map (\(a,b) -> if y1 <= a && a <= y2 then b + 1 else b) (zip [0..] r) : rs
incrementAtBoard (r:rs) x1 (y1,y2) = r : incrementAtBoard rs (x1-1) (y1,y2)

solve1 :: [ParsedLine] -> Int
solve1 = length . filter (>1) . concat . foldr applyLineOnBoard emptyBoard


applyLineOnBoard2 :: ParsedLine -> [[Int]] -> [[Int]]
applyLineOnBoard2 coords@((x1,y1), (x2,y2)) board
 | x1 == x2 = incrementAtBoard board x1 (min y1 y2, max y1 y2)
 | y1 == y2 = transpose $ incrementAtBoard (transpose board) y1 (min x1 x2, max x1 x2)
 | otherwise = incrementAtBoard2 board (x1,y1) (x2,y2) --  foldr (\(x,y) board -> incrementAtBoard board x (y,y)) board (coordsBetween coords)

incrementAtBoard2 :: [[Int]] -> (Int, Int) -> (Int, Int) -> [[Int]]
incrementAtBoard2 (r:rs) (0,y1) (0,y2) = (t ++ (d+1):ds) : rs
    where
        (t,(d:ds)) = splitAt y1 r
incrementAtBoard2 (r:rs) (0,y1) (x2,y2) = (t ++ ((d+1):ds)) : incrementAtBoard2 rs (0,y1+(signum (y2 - y1))) (x2-1,y2)
    where 
        (t,(d:ds)) = splitAt y1 r
incrementAtBoard2 (r:rs) (x1,y1) (0,y2) = (t ++ ((d+1):ds)) : incrementAtBoard2 rs (x1-1,y1) (0,y2+(signum (y1 - y2)))
    where
        (t,(d:ds)) = splitAt y2 r
incrementAtBoard2 (r:rs) (x1,y1) (x2,y2) = r : incrementAtBoard2 rs (x1-1,y1) (x2-1,y2)

coordsBetween :: ParsedLine -> [(Int, Int)]
coordsBetween ((x1,y1), (x2,y2))
 | x1 <= x2 && y1 <= y2 = zip [x1..x2] [y1..y2]
 | x1 <= x2 && y1 > y2 = zip [x1..x2] [y1,(y1-1)..y2]
 | x1 > x2 && y1 <= y2 = zip [x1,(x1-1)..x2] [y1..y2]
 | x1 > x2 && y1 > y2 = zip [x1,(x1-1)..x2] [y1,(y1-1)..y2]

solve2 :: [ParsedLine] -> Int
solve2 = length . filter (>1) . concat . foldr applyLineOnBoard2 emptyBoard