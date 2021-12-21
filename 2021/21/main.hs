module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M

type ParsedLine = Int

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
convert = read . last . words

type Die = Int
type Roll = Int
type Pos = Int
type Score = Int
type Turn = Int
type GameState = ((Pos, Pos), (Score, Score), Turn, Die)
type Game = State GameState

getDie :: GameState -> Die
getDie (_,_,_,die) = die

incDie :: GameState -> GameState
incDie (p,s,t,d) = (p,s,t,d+1)

rollDie :: Game Roll
rollDie = do
    numRolls <- gets getDie
    modify incDie
    return (numRolls `mod` 100 + 1)

rollDieN :: Int -> Game Roll
rollDieN n 
    | n <= 0 = error "Cannor roll die <=0 times"
    | n == 1 = do
        roll <- rollDie
        return roll
    | otherwise = do
        roll <- rollDie
        rest <- rollDieN (n-1)
        return (roll + rest)

getTurn :: GameState -> Turn
getTurn (_,_,turn,_) = turn

incTurn :: GameState -> GameState
incTurn (p,s,t,d) = (p,s,t+1,d)

getPlayerPos :: Int -> GameState -> Pos
getPlayerPos n ((x,y),_,_,_) = if n == 1 then x else y

updatePlayerPos :: Int -> Pos -> GameState -> GameState
updatePlayerPos n newPos ((x,y),s,t,d) = (if n == 1 then (newPos,y) else (x,newPos),s,t,d)

getPlayerScore :: Int -> GameState -> Pos
getPlayerScore n (_,(x,y),_,_) = if n == 1 then x else y

updatePlayerScore :: Int -> Score -> GameState -> GameState
updatePlayerScore n incScore (p,(x,y),t,d) = (p,if n == 1 then (x + incScore,y) else (x,y + incScore),t,d)

otherPlayer :: Int -> Int
otherPlayer 1 = 2
otherPlayer 2 = 1

movePlayer :: Roll -> Game (Maybe Score)
movePlayer roll = do
    turn <- gets getTurn
    let playerNum = fromEnum (odd turn) + 1
    playerPos <- gets (getPlayerPos playerNum)
    let newPlayerPos = (playerPos + roll) `mod` 10
    modify (updatePlayerPos playerNum newPlayerPos)
    modify (updatePlayerScore playerNum (newPlayerPos + 1))
    score <- gets (getPlayerScore playerNum)
    if score >= 1000
        then do
            otherScore <- gets (getPlayerScore (otherPlayer playerNum))
            rollCount <- gets getDie
            return . Just $ otherScore * rollCount
        else do
            return Nothing

game :: Game Score
game = do
    roll <- rollDieN 3
    result <- movePlayer roll
    if isJust result
        then 
            return (fromJust result)
        else do
            modify incTurn
            game


solve1 :: [ParsedLine] -> Int
solve1 [x,y] = evalState game startState
    where
        startState = ((x - 1, y - 1), (0, 0), 0, 0)

solve2 :: [ParsedLine] -> Int
solve2 [x,y] = max rx ry
    where
        (_, (rx, ry)) = outcomes (x-1,y-1) (0,0) 0 M.empty

type Cache = M.Map (Pos, Pos, Score, Score, Bool) (Int, Int)
outcomes :: (Pos, Pos) -> (Score, Score) -> Turn -> Cache -> (Cache, (Int, Int))
outcomes (x,y) (sx,sy) turn cache
    | sx >= 21 = (cache, (1,0))
    | sy >= 21 = (cache, (0,1))
    | M.member (x,y,sx,sy,odd turn) cache = (cache, cache M.! (x,y,sx,sy,odd turn))
    | otherwise = foldr scoreStep (cache, (0,0)) multis
    where
        playerNum = fromEnum (odd turn) + 1

        scoreStep :: (Int, Int) -> (Cache, (Int, Int)) -> (Cache, (Int, Int))
        scoreStep (roll, mult) (cch, (accx, accy)) = (M.insert (nx, ny, nsx, nsy,even turn) (rx,ry) newCache, (accx + mult * rx, accy + mult * ry))
            where
                newPoss@(nx, ny)
                    | playerNum == 1 = ((x + roll) `mod` 10, y)
                    | playerNum == 2 = (x, (y + roll) `mod` 10)
                newScores@(nsx, nsy)
                    | playerNum == 1 = (sx + fst newPoss + 1, sy)
                    | playerNum == 2 = (sx, sy + snd newPoss + 1)
                (newCache, (rx, ry)) = outcomes newPoss newScores (turn + 1) cch


multis :: [(Int, Int)]
multis = [(3,1), (4,3), (5,6), (6,7), (7,6), (8,3), (9,1)]