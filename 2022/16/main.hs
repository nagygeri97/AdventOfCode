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

type Graph2 = Map.Map (String, String) Node2

data Node2 = Node2 {
    label2 :: (String, String),
    rate2 :: (Int, Int),
    neighbors2 :: [((String, Int), (String, Int))],
    dists2 :: Array (Int, Int) Int
} deriving (Eq, Show, Ord)

type QItem2 = ((Int, Int), Int, Node2, Set.Set String) -- clock, dist, node, opens

initialDistArray2 :: Array (Int, Int) Int
initialDistArray2 = listArray (((-1),(-1)), (26,26)) (replicate (28*28) 0)

getNewNode2 :: (String, String) -> [String] -> Map.Map String InputGraph -> Node2
getNewNode2 (label1, label2) labelsWithRate bfsGs = Node2 (label1, label2) (rt currentNode1, rt currentNode2) neighbors initialDistArray2
    where
        currentNode1 = (bfsGs Map.! label1) Map.! label1
        currentNode2 = (bfsGs Map.! label2) Map.! label2
        neighbors1 = map ((\inode -> (lbl inode, dist inode)) . ((bfsGs Map.! label1) Map.!)) labelsWithRate
        neighbors2 = map ((\inode -> (lbl inode, dist inode)) . ((bfsGs Map.! label2) Map.!)) labelsWithRate
        neighbors = [(n1, n2) | n1 <- neighbors1, n2 <- neighbors2]

getNewNodes2 :: String -> [String] -> InputGraph -> [Node2]
getNewNodes2 label labelsWithRate inputG = newNodes
    where
        initialGsWithLabel = zip labelsWithRate $ map (\l -> Map.update (Just . setDist 0) l inputG) labelsWithRate
        bfsGs = foldr (\(l,g) m -> Map.insert l g m) Map.empty $ map (\(l, initialG) -> (l, bfs initialG (initialG Map.! l) [])) initialGsWithLabel
        allNewLabels = [(l1, l2) | l1 <- labelsWithRate, l2 <- labelsWithRate]
        newNodes = map (\ls -> getNewNode2 ls labelsWithRate bfsGs) allNewLabels

getNewGraph2 :: [String] -> InputGraph -> Graph2
getNewGraph2 labelsWithRate inputG = newG
    where
        newNodes = concat $ map (\label -> getNewNodes2 label labelsWithRate inputG) labelsWithRate
        newG = foldr (\node g -> Map.insert (label2 node) node g) Map.empty newNodes

newDist2 :: Graph2 -> ((String, Int), (String, Int)) -> Int -> (Int, Int) -> (Int, Int)
newDist2 g ((label1, time1), (label2, time2)) dist (clock1, clock2) = (distCurr, dist + additionalScore)
    where
        additionalScore1 = if c1 == (-1) then 0 else ((clock1 + time1 + 1) - 26) * r1
        additionalScore2 = if c2 == (-1) then 0 else ((clock2 + time2 + 1) - 26) * r2
        additionalScore = if label1 == label2 then max additionalScore1 additionalScore2 else additionalScore1 + additionalScore2
        c1 = addClockTimeWithCap clock1 time1
        c2 = addClockTimeWithCap clock2 time2
        distCurr = distArr ! (c1, c2)
        distArr = dists2 node
        (r1, r2) = rate2 node
        node = g Map.! (label1, label2)

addClockTimeWithCap :: Int -> Int -> Int
addClockTimeWithCap clock time
    | clock < 0 || clock + time + 1 > 26 = (-1)
    | otherwise = clock + time + 1

isValidNb :: (Int, Int) -> (Int, Int) -> (String, String) -> Set.Set String -> Bool
isValidNb (clock1, clock2) (time1, time2) (label1, label2) opens
    | t1 && nopen1 && t2 && nopen2 = True
    | t1 && nopen1 && not t2 = True
    | not t1 && t2 && nopen2 = True
    | otherwise = False
    where
        c1 = addClockTimeWithCap clock1 time1
        t1 = c1 < 26 && c1 >= 0
        nopen1 = not $ Set.member label1 opens
        c2 = addClockTimeWithCap clock2 time2
        t2 = c2 < 26 && c2 >= 0
        nopen2 = not $ Set.member label2 opens

createQItem :: (((String, Int), (String, Int)), (Int, Int)) -> (Int, Int) -> Set.Set String -> Graph2 -> QItem2
createQItem (((label1, time1), (label2, time2)), (_, newDist)) (clock1, clock2) opens g = ((c1, c2), newDist, g Map.! (label1, label2), newSet2)
    where
        c1 = addClockTimeWithCap clock1 time1
        c2 = addClockTimeWithCap clock2 time2
        newSet1 = if c1 == (-1) then opens else Set.insert label1 opens
        newSet2 = if c2 == (-1) then newSet1 else Set.insert label2 newSet1

findPath2 :: Graph2 -> QItem2 -> PQueue.MinQueue QItem2 -> Graph2
findPath2 g (clock@(clock1, clock2), dist, node, opens) pq
    | PQueue.null newQ = g
    | otherwise = findPath2 newG (PQueue.findMin newQ) (PQueue.deleteMin newQ)
    where
        newDists = map (\nb -> (nb, newDist2 g nb dist clock)) . filter (\((label1, time1), (label2, time2)) -> isValidNb (clock1, clock2) (time1, time2) (label1, label2) opens) $ (neighbors2 node)
        improvedDists = filter (\(nb, (distCurr, newDist)) -> newDist < distCurr) newDists
        newQItems = map (\it -> createQItem it (clock1, clock2) opens g) improvedDists
        newG = foldr (\(c, d, n, _) graph -> Map.insert (label2 n) (setDistance2 c d n) graph) g newQItems
        newQ = foldr PQueue.insert pq newQItems

setDistance2 :: (Int, Int) -> Int -> Node2 -> Node2
setDistance2 c d (Node2 l r n ds) = Node2 l r n (ds // [(c,d)])

solve2 :: [ParsedLine2] -> Int
solve2 lines = negate . minimum . concat . map (elems . dists2) . Map.elems $ path
    where 
        path = findPath2 newG ((0, 0), 0, newG Map.! ("AA", "AA"), Set.empty) PQueue.empty
        newG = getNewGraph2 ("AA":labelsWithRate) inputG 
        labelsWithRate = valvesWithRate inputG
        inputG = toInputGraph lines