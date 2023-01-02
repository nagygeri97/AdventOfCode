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

type Visited = Array (Int, Int) Bool
type VisitedPerGraph = Map.Map Graph Visited

checkVisited :: VisitedPerGraph -> Graph -> (Int, Int) -> Bool
checkVisited vpg g p = Map.member g vpg && (vpg Map.! g) ! p

updateVisited :: Graph -> VisitedPerGraph -> (Int, Int) -> VisitedPerGraph
updateVisited g vpg p
    | Map.member g vpg = Map.update (\v -> Just $ v // [(p, False)]) g vpg
    | otherwise = updateVisited g (Map.insert g (listArray (bounds g) [False | _ <- elems g]) vpg) p

simulate :: Int -> (Int, Int) -> [((Int, Int), Int)] -> VisitedPerGraph -> Graph -> Int
simulate m goal ((pos, n):poss) vpg g
    | goal == pos = (n - 1)
    | otherwise = simulate n goal (poss ++ movesWithN) newVpg nextG
    where
        nextG = if m == n then g else nextGraph g
        moves = filter (not . checkVisited vpg g) $ possibleNextMoves nextG pos
        movesWithN = [(move, n+1) | move <- moves]
        newVpg = foldl (updateVisited nextG) vpg moves

indexOf :: Eq a => a -> [a] -> Int
indexOf e (x:xs)
    | e == x = 0
    | otherwise = 1 + indexOf e xs

convert :: String -> ParsedLine
convert = map toField

convert2 :: String -> ParsedLine2
convert2 = convert

solve1 :: [ParsedLine] -> Int
solve1 lines = simulate 0 goal [(start, 1)] Map.empty graph
    where
        goal = (length lines - 1, indexOf (B []) (last lines))
        start = (0, indexOf (B []) (head lines))
        graph = toGraph lines

solve2 :: [ParsedLine2] -> Int
solve2 = undefined