module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative

type ParsedLine = String

type Arr = Array (Int,Int) Field

data Field = Empty | East | South deriving (Eq, Ord, Enum, Show)

main :: IO ()
main = mainLoop []

mainLoop :: [ParsedLine] -> IO ()
mainLoop xs = do
    done <- isEOF
    if done
        then do putStrLn . ("Part One: " ++) . show . solve1 $ listListToArray $ (map . map) toField xs
        else do input <- pure convert <*> getLine
                mainLoop (xs ++ [input])

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim list
    | null last = [first]
    | otherwise = first : splitBy delim (tail last)
    where
        (first,last) = break (==delim) list

listListToArray :: [[a]] -> Array (Int, Int) a
listListToArray xs = array ((0,0), (maxx, maxy)) (zip allIdx (concat xs))
    where
        maxx = length xs - 1
        maxy = length (head xs) - 1
        allIdx = [(x,y) | x <- [0..maxx], y<-[0..maxy]]

toField :: Char -> Field
toField '.' = Empty
toField '>' = East
toField 'v' = South

convert :: String -> ParsedLine
convert = id

arraySize :: Arr -> (Int, Int)
arraySize = snd . bounds

right :: Arr -> (Int, Int) -> (Int, Int)
right arr (x, y) = (x, if y == maxy then 0 else (y + 1))
    where
        (maxx, maxy) = arraySize arr

down :: Arr -> (Int, Int) -> (Int, Int)
down arr (x, y) = (if x == maxx then 0 else (x + 1), y)
    where
        (maxx, maxy) = arraySize arr

canMove :: (Arr -> (Int, Int) -> (Int, Int)) -> Arr -> ((Int, Int), Field) -> Bool
canMove f arr (idx, _) = arr ! (f arr idx) == Empty

move :: Field -> (Arr -> (Int, Int) -> (Int, Int)) -> Arr -> (Int, Arr)
move field f arr = (length shouldMove, arr // toEmptyList // toMove)
    where
        shouldMove = filter (canMove f arr) . filter ((==field) . snd) $ assocs arr
        toEmptyList = map (\(idx,_) -> (idx, Empty)) shouldMove
        toMove = map (\(idx,x) -> (f arr idx, x)) shouldMove

completeMove :: Arr -> (Int, Arr)
completeMove arr = (n + m, arr2)
    where
        (n, arr1) = move East right arr
        (m, arr2) = move South down arr1

moveUntilDone :: Arr -> Int -> Int
moveUntilDone arr rounds
    | n == 0 = rounds
    | otherwise = moveUntilDone arr1 (rounds + 1)
    where
        (n, arr1) = completeMove arr

solve1 :: Arr -> Int
solve1 arr = moveUntilDone arr 1

solve2 :: Arr -> Int
solve2 = undefined