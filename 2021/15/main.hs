module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative
import qualified Data.PQueue.Min as PQueue
import qualified Data.Set as Set 

type ArrayInt a = Array Int a

type ParsedLine = ArrayInt Int
type Arr = ArrayInt ParsedLine

main :: IO ()
main = mainLoop []

mainLoop :: [ParsedLine] -> IO ()
mainLoop xs = do
    done <- isEOF
    if done
        then do putStrLn . ("Part One: " ++) . show . solve1 $ listToArray xs
                putStrLn . ("Part Two: " ++) . show . solve2 $ listToArray xs
        else do input <- pure convert <*> getLine
                mainLoop (xs ++ [input])

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim list
    | null last = [first]
    | otherwise = first : splitBy delim (tail last)
    where
        (first,last) = break (==delim) list

listToArray :: [a] -> ArrayInt a
listToArray xs = listArray (0, length xs - 1) xs

convert :: String -> ParsedLine
convert = listToArray . map (\x -> read [x])

sizes :: Arr -> (Int, Int)
sizes arr = (snd . bounds $ arr, snd . bounds $ arr ! 0)

neighbours :: Arr -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
neighbours arr (x,y) (maxx, maxy) = filter (\(x,y) -> x >= 0 && x <= maxx && y >= 0 && y <= maxy) 
    [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

solve1 :: Arr -> Int
solve1 arr = dijkstra arr q end visited
    where
        start = (0,0)
        end = sizes arr
        q = PQueue.singleton (0, start)
        visited = Set.empty

type QueueItem = (Int, (Int, Int))
type Queue = PQueue.MinQueue QueueItem
type Visited = Set.Set (Int, Int)

get :: Arr -> (Int, Int) -> Int
get arr (x,y) = value
    where
        (maxx, maxy) = sizes arr
        offx = x `div` (maxx + 1)
        offy = y `div` (maxy + 1)
        nx = x `mod` (maxx + 1)
        ny = y `mod` (maxy + 1)
        value = (((arr ! nx ! ny + offx + offy) - 1) `mod` 9) + 1 


dijkstra :: Arr -> Queue -> (Int, Int) -> Visited -> Int
dijkstra arr q end visited
    | curr == end = cost
    | Set.member curr visited = dijkstra arr newQ end visited
    | otherwise = dijkstra arr finalQ end newVisited
    where
        ((cost, curr), newQ) = PQueue.deleteFindMin q
        ns = neighbours arr curr end
        newQueueItems = zip (map ((+cost) . get arr) ns) ns
        finalQ = PQueue.union newQ (PQueue.fromList newQueueItems)
        newVisited = Set.insert curr visited


solve2 :: Arr -> Int
solve2 arr = dijkstra arr q end visited
    where
        start = (0,0)
        (maxx, maxy) = sizes arr
        end = (5*(maxx + 1) - 1, 5*(maxy + 1) - 1)
        q = PQueue.singleton (0, start)
        visited = Set.empty