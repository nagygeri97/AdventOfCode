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
import qualified Data.Map as Map
import qualified Data.Set as Set

data Field = Wall | B [Blizzard] deriving (Eq, Ord, Show)
data Blizzard
    = L
    | R
    | U
    | D
    deriving (Eq, Ord, Show)

type Graph = Array (Int, Int) Field

type ParsedLine = [Field]

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

toField :: Char -> Field
toField '.' = B []
toField '#' = Wall
toField '<' = B [L]
toField '>' = B [R]
toField '^' = B [U]
toField 'v' = B [D]

toGraph :: [[Field]] -> Graph
toGraph fields = listArray ((0, 0), (maxx, maxy)) . concat $ fields
    where
        maxx = length fields - 1
        maxy = length (head fields) - 1

nextBlizzardPos :: Graph -> Blizzard -> (Int, Int) -> (Int, Int)
nextBlizzardPos g b (x, y)
    | g ! possiblePos == Wall = wrappedPos
    | otherwise = possiblePos
    where
        ((minx, miny), (maxx, maxy)) = bounds g
        possiblePos = case b of
            L -> (x, y-1)
            R -> (x, y+1)
            U -> (x-1, y)
            D -> (x+1, y)
        wrappedPos = case b of
            L -> (x, maxy-1)
            R -> (x, miny+1)
            U -> (maxx-1, y)
            D -> (minx+1, y)

makeEmpty :: Field -> Field
makeEmpty Wall = Wall
makeEmpty (B _) = B []

nextGraph :: Graph -> Graph
nextGraph g = newG
    where
        idxFields = assocs g
        emptyG = array (bounds g) . map (\(idx, field) -> (idx, makeEmpty field)) $ idxFields
        newBlizards = concat . map (\(idx, field) -> case field of Wall -> []; B bs -> map (\b -> (nextBlizzardPos g b idx, b)) bs) $ idxFields
        newG = accum (\field b -> case field of Wall -> Wall; B bs -> B (b:bs)) emptyG newBlizards

possibleNextMoves :: Graph -> (Int, Int) -> [(Int, Int)]
possibleNextMoves nextG (x, y) = moves
    where
        ((minx, miny), (maxx, maxy)) = bounds nextG
        moves = filter (\m -> nextG ! m == (B [])) . filter (\(a,b) -> a >= minx && a <= maxx && b >= miny && b <= maxy) $ [(x+1, y), (x-1, y), (x,y), (x, y+1), (x, y-1)]

type Visited = Array (Int, Int, Int) Bool

checkVisited :: Visited -> Int -> (Int, Int) -> Bool
checkVisited v g (x,y) = v ! (g, x, y)

simulate :: Int -> Int -> (Int, Int) -> Set.Set (Int, Int) -> Visited -> Array Int Graph -> Int
simulate n maxGIdx goal poss vpg gs
    | Set.member goal poss = n
    | otherwise = simulate (n + 1) maxGIdx goal newPoss newVpg gs
    where
        posList = Set.toList poss
        newPoss = Set.fromList . concat . map (possibleNextMoves nextG) . filter (not . checkVisited vpg gIdx) $ posList 

        gIdx = n `mod` maxGIdx
        nextGIdx = (n + 1) `mod` maxGIdx
        nextG = gs ! nextGIdx

        newVpg = vpg // [((gIdx, x, y), True) | (x,y) <- posList]

indexOf :: Eq a => a -> [a] -> Int
indexOf e (x:xs)
    | e == x = 0
    | otherwise = 1 + indexOf e xs

allGraphs :: Graph -> Int -> Int -> [(Int, Graph)]
allGraphs g i maxi
    | i == maxi = [] 
    | otherwise = (i, g) : allGraphs (nextGraph g) (i + 1) maxi

convert :: String -> ParsedLine
convert = map toField

convert2 :: String -> ParsedLine2
convert2 = convert

solve1 :: [ParsedLine] -> Int
solve1 lines = simulate 0 maxGIdx goal (Set.singleton start) emptyVisited allgs
    where
        goal = (height - 1, indexOf (B []) (last lines))
        start = (0, indexOf (B []) (head lines))
        graph = toGraph lines

        height = length lines
        width = length (head lines)
        maxGIdx = lcm (height - 2) (width - 2)

        emptyVisited = listArray ((0,0,0), (maxGIdx - 1, height - 1, width - 1)) (repeat False)

        allgs = array (0, maxGIdx - 1) $ allGraphs graph 0 maxGIdx

solve2 :: [ParsedLine2] -> Int
solve2 lines = goBackToEnd
    where
        goal = (height - 1, indexOf (B []) (last lines))
        start = (0, indexOf (B []) (head lines))
        graph = toGraph lines

        height = length lines
        width = length (head lines)
        maxGIdx = lcm (height - 2) (width - 2)

        emptyVisited = listArray ((0,0,0), (maxGIdx - 1, height - 1, width - 1)) (repeat False)

        allgs = array (0, maxGIdx - 1) $ allGraphs graph 0 maxGIdx

        goToEnd = simulate 0 maxGIdx goal (Set.singleton start) emptyVisited allgs
        goBackToStart = simulate goToEnd maxGIdx start (Set.singleton goal) emptyVisited allgs
        goBackToEnd = simulate goBackToStart maxGIdx goal (Set.singleton start) emptyVisited allgs