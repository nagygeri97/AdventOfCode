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
import qualified Data.Set as Set
import qualified Data.Map as Map

data Dir = L | R deriving (Eq, Ord, Show)
type ParsedLine = [Dir]

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

arrayFromList :: [a] -> Array Int a
arrayFromList xs = listArray (0, length xs - 1) xs

convert :: String -> ParsedLine
convert [] = []
convert ('<':xs) = L:convert xs
convert ('>':xs) = R:convert xs

initialShapes :: [Set.Set (Int, Int)]
initialShapes = [
    Set.fromList [(0,2), (0,3), (0,4), (0,5)], 
    Set.fromList [(0,3), (1,2), (1,3), (1,4), (2,3)],
    Set.fromList [(0,2), (0,3), (0,4), (1,4), (2,4)],
    Set.fromList [(0,2), (1,2), (2,2), (3,2)],
    Set.fromList [(0,2), (0,3), (1,2), (1,3)]
    ]

type S = ([Dir], Set.Set (Int, Int), Set.Set (Int, Int), [Set.Set (Int, Int)], Int) -- moves, tower, falling rock, shapes, settled counter

putShapeUp :: Set.Set (Int, Int) -> Set.Set (Int, Int) -> Set.Set (Int, Int)
putShapeUp tower shape = Set.map (\(a,b) -> (a + maxHeight + 4, b)) shape
    where
        (maxHeight, _) = Set.findMax tower

increaseCounter :: S -> S
increaseCounter (moves, tower, falling, shapes, counter) = (moves, tower, falling, shapes, counter + 1)

addRockToTower :: Set.Set (Int, Int) -> S -> S
addRockToTower falling (moves, tower, _, shapes, counter) = (moves, Set.union falling tower, Set.empty, shapes, counter)

getNextRock :: S -> S
getNextRock (moves, tower, _, (shape:shapes), counter) = (moves, tower, putShapeUp tower shape, shapes, counter)

removeMove :: S -> S
removeMove ((_:moves), tower, falling, shapes, counter) = (moves, tower, falling, shapes, counter)

updateFalling :: Set.Set (Int, Int) -> S -> S
updateFalling falling (moves, tower, _, shapes, counter) = (moves, tower, falling, shapes, counter)

settleRock :: Set.Set (Int, Int) -> State S ()
settleRock falling = do
    modify increaseCounter
    modify (addRockToTower falling)
    modify getNextRock

performPush :: Dir -> Set.Set (Int, Int) -> Set.Set (Int, Int) -> Set.Set (Int, Int)
performPush dir shape tower
    | outOfBounds || collided = shape
    | otherwise = newShape
    where 
        newShape = Set.map (\(a,b) -> (a, b + if dir == R then 1 else (-1))) shape
        outOfBounds = not . Set.null . Set.filter (\(a,b) -> b < 0 || b > 6) $ newShape
        collided = not . Set.null $ Set.intersection newShape tower

performFall :: Set.Set (Int, Int) -> Set.Set (Int, Int)
performFall = Set.map (\(a,b) -> (a - 1, b))

advance :: State S Int
advance = do
    (moves, tower, falling, shapes, counter) <- get
    if counter == 2022
        then return (fst (Set.findMax tower))
        else do
            let pushedFalling = performPush (head moves) falling tower
            modify removeMove
            let fellFalling = performFall pushedFalling
            if Set.null (Set.intersection tower fellFalling)
                then modify (updateFalling fellFalling)
                else settleRock pushedFalling
            advance

initialTower :: Set.Set (Int, Int)
initialTower = Set.fromList [(0,0), (0,1), (0,2), (0,3), (0,4), (0,5), (0,6)]

convert2 :: String -> ParsedLine2
convert2 = convert

solve1 :: [ParsedLine] -> Int
solve1 [line] = evalState advance initialState
    where
        moves = cycle line
        (shape:shapes) = cycle initialShapes

        initialState = (moves, initialTower, putShapeUp initialTower shape, shapes, 0)

displayList :: [String] -> IO ()
displayList [] = return ()
displayList (x:xs) = do
    putStrLn x
    displayList xs

displayAndRunTower :: [ParsedLine2] -> IO ()
displayAndRunTower [line] = displayList (reverse dt)
    where
        moves = cycle line
        (shape:shapes) = cycle initialShapes

        initialState = ((moves, length line), initialTower, putShapeUp initialTower shape, shapes, 0, Map.empty, Map.empty)

        (_, tower, _, _, _, _, _) = snd $ runState advance2 initialState
        dt = displayTower tower 1

solve2 :: [ParsedLine2] -> Int
solve2 [line] = evalState advance2 initialState
    where
        moves = cycle line
        (shape:shapes) = cycle initialShapes
        initialState = ((moves, length line), initialTower, putShapeUp initialTower shape, shapes, 0, Map.empty, Map.empty)


displayTower :: Set.Set (Int, Int) -> Int -> [String]
displayTower tower level
    | Set.null currentLevel = []
    | otherwise = line : displayTower tower (level + 1)
    where 
        currentLevel = Set.filter (\(a,b) -> a == level) tower
        currentIndices = Set.map snd currentLevel
        line = [if Set.member k currentIndices then '#' else '.' | k <- [0..6]]


type Env = ([Dir], [Set.Set (Int, Int)], Set.Set (Int, Int)) -- moves, shapes, tower
type S2 = (([Dir], Int), Set.Set (Int, Int), Set.Set (Int, Int), [Set.Set (Int, Int)], Int, Map.Map Env (Int, Int), Map.Map Int Int) 
-- moves, tower, falling rock, shapes, settled counter, states, heights

increaseCounter2 :: S2 -> S2
increaseCounter2 (moves, tower, falling, shapes, counter, states, heights) = (moves, tower, falling, shapes, counter + 1, states, heights)

addRockToTower2 :: Set.Set (Int, Int) -> S2 -> S2
addRockToTower2 falling (moves, tower, _, shapes, counter, states, heights) = (moves, Set.union falling tower, Set.empty, shapes, counter, states, heights)

getNextRock2 :: S2 -> S2
getNextRock2 (moves, tower, _, (shape:shapes), counter, states, heights) = (moves, tower, putShapeUp tower shape, shapes, counter, states, heights)

removeMove2 :: S2 -> S2
removeMove2 ((_:moves, len), tower, falling, shapes, counter, states, heights) = ((moves, len), tower, falling, shapes, counter, states, heights)

updateFalling2 :: Set.Set (Int, Int) -> S2 -> S2
updateFalling2 falling (moves, tower, _, shapes, counter, states, heights) = (moves, tower, falling, shapes, counter, states, heights)

settleRock2 :: Set.Set (Int, Int) -> State S2 ()
settleRock2 falling = do
    modify increaseCounter2
    modify (addRockToTower2 falling)
    modify getNextRock2

getLatestNLayers :: Int -> Set.Set (Int, Int) -> Set.Set (Int, Int)
getLatestNLayers n tower = normalizedLayers
    where
        normalizedLayers = Set.map (\(a, b) -> (a + n - maxHeight, b)) lastNLayers
        lastNLayers = Set.filter (\(a, b) -> a > maxHeight - n - 1) tower
        (maxHeight, _) = Set.findMax tower

getCurrentState :: ([Dir], Int) -> Set.Set (Int, Int) -> [Set.Set (Int, Int)] -> Env
getCurrentState (moves, len) tower shapes = (take len moves, take 5 shapes, getLatestNLayers 20 tower)

updateState :: Env -> S2 -> S2
updateState env (moves, tower, falling, shapes, counter, states, heights) = (moves, tower, falling, shapes, counter, Map.insert env (counter, height) states, Map.insert counter height heights)
    where
        (height, _) = Set.findMax tower

advance2 :: State S2 Int
advance2 = do
    ((moves, len), tower, falling, shapes, counter, states, heights) <- get
    let currentState = getCurrentState (moves, len) tower shapes
    if Map.member currentState states
        then do
            let (oldCounter, oldHeight) = states Map.! currentState
            let (currentHeight, _) = Set.findMax tower
            let cycleLength = counter - oldCounter
            let settledBeforeCycle = oldCounter
            let heightBeforeCycle = oldHeight
            let cycleHeight = currentHeight - oldHeight
            let settlesNeeded = 1000000000000 - settledBeforeCycle
            let cycleCountForSettlesNeeded = settlesNeeded `div` cycleLength
            let settlesNeededAfterCycles = settlesNeeded `mod` cycleLength
            let heightFromCycles = cycleCountForSettlesNeeded * cycleHeight
            let heightAfterCycles = (heights Map.! (oldCounter + settlesNeededAfterCycles)) - oldHeight
            return $ heightBeforeCycle + heightFromCycles + heightAfterCycles
        else do
            modify (updateState currentState)
            let pushedFalling = performPush (head moves) falling tower
            modify removeMove2
            let fellFalling = performFall pushedFalling
            if Set.null (Set.intersection tower fellFalling)
                then modify (updateFalling2 fellFalling)
                else settleRock2 pushedFalling
            advance2