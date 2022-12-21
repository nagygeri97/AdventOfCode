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
import qualified Data.PQueue.Min as PQueue

type ParsedLine = InputNode

type ParsedLine2 = ParsedLine

type InputGraph = Map.Map String InputNode

data InputNode = IN {
    lbl :: String,
    rt :: Int,
    nbs :: [String],
    dist :: Int
} deriving (Eq, Show)


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

removeComma :: String -> String
removeComma [] = []
removeComma (',':xs) = xs
removeComma (x:xs) = x:removeComma xs

convert :: String -> ParsedLine
convert line = IN label rate neighborsList 999
    where
        neighborsList = map removeComma neighbors
        rate = read $ filter isDigit rateStr
        (_:label:_:_:rateStr:_:_:_:_:neighbors) = splitBy ' ' line

toInputGraph :: [InputNode] -> InputGraph
toInputGraph = Map.fromList . map (\node -> (lbl node, node))

valvesWithRate :: InputGraph -> [String]
valvesWithRate = map lbl . filter (\node -> rt node > 0) . Map.elems

setDist :: Int -> InputNode -> InputNode
setDist n (IN label rate neighbors _) = IN label rate neighbors n

bfs :: InputGraph -> InputNode -> [InputNode] -> InputGraph
bfs g currentNode q
    | null newQ = g
    | otherwise = bfs newG (head newQ) (tail newQ)
    where
        newG = foldr (\node graph -> Map.insert (lbl node) node graph) g nNodes
        newQ = q ++ nNodes
        nNodes = map (setDist (dist currentNode + 1)) $ filter (\node -> dist node > dist currentNode) $ map (g Map.!) neighbors
        neighbors = nbs currentNode

initialDistArray :: Array Int Int
initialDistArray = listArray (0,30) (replicate 31 0)

getNewNode :: String -> [String] -> InputGraph -> Node
getNewNode label labelsWithRate inputG = Node label (rt currentNode) neighbors initialDistArray
    where
        currentNode = initialG Map.! label
        initialG = Map.update (Just . setDist 0) label inputG
        bfsIG = bfs initialG (initialG Map.! label) []
        neighbors = map ((\inode -> (lbl inode, dist inode)) . (bfsIG Map.!)) labelsWithRate

getNewGraph :: [String] -> InputGraph -> Graph
getNewGraph labelsWithRate inputG = newG
    where
        newNodes = map (\label -> getNewNode label labelsWithRate inputG) labelsWithRate
        newG = foldr (\node g -> Map.insert (label node) node g) Map.empty newNodes

type Graph = Map.Map String Node

data Node = Node {
    label :: String,
    rate :: Int,
    neighbors :: [(String, Int)],
    dists :: Array Int Int
} deriving (Eq, Show, Ord)

type QItem = (Int, Int, Node, Set.Set String) -- clock, dist, node, opens

newDist :: Graph -> (String, Int) -> Int -> Int -> (Int, Int)
newDist g (label, time) dist clock = (distCurr, dist + additionalScore)
    where
        additionalScore = ((clock + time + 1) - 30) * (rate node)
        distCurr = distArr ! (clock + time + 1)
        distArr = dists node
        node = g Map.! label

setDistance :: Int -> Int -> Node -> Node
setDistance c d (Node l r n ds) = Node l r n (ds // [(c,d)])

findPath :: Graph -> QItem -> PQueue.MinQueue QItem -> Graph
findPath g (clock, dist, node, opens) pq
    | PQueue.null newQ = g
    | otherwise = findPath newG (PQueue.findMin newQ) (PQueue.deleteMin newQ)
    where
        newDists = map (\nb -> (nb, newDist g nb dist clock)) . filter (\(label, time) -> clock + time + 1 <= 30 && not (Set.member label opens)) $ (neighbors node)
        improvedDists = filter (\(nb, (distCurr, newDist)) -> newDist < distCurr) newDists
        newQItems = map (\((label, time), (_, newDist)) -> (clock + time + 1, newDist, g Map.! label, Set.insert label opens)) improvedDists
        newG = foldr (\(c, d, n, _) graph -> Map.insert (label n) (setDistance c d n) graph) g newQItems
        newQ = foldr PQueue.insert pq newQItems


convert2 :: String -> ParsedLine2
convert2 = convert

solve1 :: [ParsedLine] -> Int
solve1 lines = negate . minimum . concat . map (elems . dists) . Map.elems $ path
    where 
        path = findPath newG (0, 0, newG Map.! "AA", Set.empty) PQueue.empty
        newG = getNewGraph ("AA":labelsWithRate) inputG 
        labelsWithRate = valvesWithRate inputG
        inputG = toInputGraph lines

solve2 :: [ParsedLine2] -> Int
solve2 = undefined