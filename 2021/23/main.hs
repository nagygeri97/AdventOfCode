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
import qualified Data.Map as Map

type ParsedLine = String

data Amphipod = A | B | C | D deriving (Eq, Ord, Enum, Show)

type Home = Map.Map Int Amphipod

type Corridor = Map.Map Int Amphipod

type Board = (Corridor, [Home])

type Cost = Int

type Item = (Cost, Board)

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
convert = id

convertInput :: [ParsedLine] -> Board
convertInput lines = (corridor, homes)
    where
        corridor = Map.empty
        homes = map (Map.fromList) . map (zip [0..]). splitSizes 2 . map alphaToAmphipod . filter isAlpha . concat . transpose $ lines

homePos :: Amphipod -> Int
homePos = (*2) . (+1) . fromEnum

homeIndexes :: [Int]
homeIndexes = [2,4,6,8]

getFreePosFromHome :: Corridor -> Int -> [Int]
getFreePosFromHome corridor homeInd = sort all
    where
        before = tail $ until (\(x:xs) -> x `elem` occupied || x < 0) (\l@(x:xs) -> (x-1):l) [homeInd - 1]
        after = tail $ until (\(x:xs) -> x `elem` occupied || x > 10) (\l@(x:xs) -> (x+1):l) [homeInd + 1]
        occupied = Map.keys corridor
        all = (before ++ after) \\ homeIndexes

movesOut :: Item -> [Item]
movesOut item@(cost, (corridor, homes)) = newMoves
    where
        frees = map (getFreePosFromHome corridor) homeIndexes
        indexFrees = zip [0..] frees
        newMoves = concatMap (\(n, frees) -> concatMap (\k -> moveOutFromNthHomeToKthPos n k item) frees) indexFrees

moveOutFromNthHomeToKthPos :: Int -> Int -> Item -> [Item]
moveOutFromNthHomeToKthPos n k (cost, (corridor, homes))
    | Map.null nthHome = []
    | otherwise = [(cost + extraCost, (newCorridor, newHomes))]
    where
        nthHome = homes !! n
        firstKey = head $ Map.keys nthHome
        firstAmphi = nthHome Map.! firstKey
        newCorridor = Map.insert k firstAmphi corridor
        newHomes = removeKeyFromNthHome firstKey n homes

        extraCost = getCost firstAmphi * ((firstKey + 1) + abs (((n+1)*2) - k))

removeKeyFromNthHome :: Int -> Int -> [Home] -> [Home]
removeKeyFromNthHome key n homes = t ++ newHome : d
    where
        (t,(nthHome:d)) = splitAt n homes 
        newHome = Map.delete key nthHome

isHomeOkToMoveIn :: Amphipod -> Home -> Int -> Bool
isHomeOkToMoveIn x home maxHomeSize = all (`notElem` keys) leftKeys && stangers == Map.empty
    where
        keys = Map.keys home
        keyCount = length keys
        leftKeys = [0..(maxHomeSize - keyCount - 1)]

        stangers = Map.filter (/=x) home

posToHomeIndex :: Int -> Int
posToHomeIndex p = (p `div` 2) - 1

movesIn :: Int -> Item -> [Item]
movesIn maxHomeSize item@(cost, (corridor, homes)) = allMoves
    where
        possibleMoves = map (\(from, to) -> (from, posToHomeIndex to)). filter (isFreeBetween corridor) . map (\(pos,x) -> (pos,homePos x)) . Map.assocs $ corridor
        allMoves = concatMap (\(k,n) -> moveInFromKthPosToNthHome maxHomeSize k n item) possibleMoves

isFreeBetween :: Corridor -> (Int, Int) -> Bool
isFreeBetween corridor (x,y)
    | x <= y = all (not . flip Map.member corridor) [(x+1)..y]
    | otherwise =  all (not . flip Map.member corridor) [y..(x-1)]


moveInFromKthPosToNthHome :: Int -> Int -> Int -> Item -> [Item]
moveInFromKthPosToNthHome  maxHomeSize k n (cost, (corridor, homes))
    | isHomeOkToMoveIn amphi nthHome maxHomeSize = [(cost + extraCost, (newCorridor, newHomes))]
    | otherwise = []
    where
        nthHome = homes !! n
        amphi = corridor Map.! k
        newKey = (if nthHome == Map.empty then maxHomeSize else head (Map.keys nthHome)) - 1
        newNthHome = Map.insert newKey amphi nthHome 
        
        newHomes = replaceNth n newNthHome homes
        newCorridor = Map.delete k corridor

        extraCost = getCost amphi * ((newKey + 1) + abs (((n+1)*2) - k))

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n e xs = t ++ e:d
    where
        (t,(_:d)) = splitAt n xs

splitSizes :: Int -> [a] -> [[a]]
splitSizes n [] = []
splitSizes n xs = t : splitSizes n d
    where
        (t,d) = splitAt n xs

getCost :: Amphipod -> Int
getCost A = 1
getCost B = 10
getCost C = 100
getCost D = 1000

alphaToAmphipod :: Char -> Amphipod
alphaToAmphipod 'A' = A
alphaToAmphipod 'B' = B
alphaToAmphipod 'C' = C
alphaToAmphipod 'D' = D

nextMoves :: Int -> Item -> [Item]
nextMoves maxHomeSize item = movesOut item ++ movesIn maxHomeSize item

isEnd :: Int -> Board -> Bool
isEnd maxHomeSize (corridor, homes) = all (checkHome maxHomeSize) $ zip [(A)..(D)] homes

checkHome :: Int -> (Amphipod, Home) -> Bool
checkHome maxHomeSize (amphi, home) = Map.filter (/=amphi) home == Map.empty && Map.size home == maxHomeSize

type Queue = PQueue.MinQueue Item
type Visited = Set.Set Board

dijkstra :: Int -> Queue -> Visited -> Int
dijkstra maxHomeSize q visited
    | isEnd maxHomeSize curr = cost
    | Set.member curr visited = dijkstra maxHomeSize newQ visited
    | otherwise = dijkstra maxHomeSize finalQ newVisited
    where
        (item@(cost, curr), newQ) = PQueue.deleteFindMin q
        newQueueItems = nextMoves maxHomeSize item
        finalQ = PQueue.union newQ (PQueue.fromList newQueueItems)
        newVisited = Set.insert curr visited


solve1 :: [ParsedLine] -> Int
solve1 lines = dijkstra 2 (PQueue.singleton (0, initBoard)) Set.empty
    where
        initBoard = convertInput lines

convertInput2 :: [ParsedLine] -> Board
convertInput2 lines = (corridor, homes)
    where
        newLines = insertAfter 3 lines ["  #D#C#B#A#", "  #D#B#A#C#"] 
        corridor = Map.empty
        homes = map (Map.fromList) . map (zip [0..]). splitSizes 4 . map alphaToAmphipod . filter isAlpha . concat . transpose $ newLines

insertAfter :: Int -> [a] -> [a] -> [a]
insertAfter n xs ys = t ++ ys ++ d
    where
        (t,d) = splitAt n xs

solve2 :: [ParsedLine] -> Int
solve2 lines = dijkstra 4 (PQueue.singleton (0, initBoard)) Set.empty
    where
        initBoard = convertInput2 lines