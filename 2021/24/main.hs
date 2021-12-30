module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import Data.Either
import System.IO
import Control.Applicative
import qualified Data.Map as Map

type ParsedLine = Instruction

data Variable = X | Y | Z | W deriving (Eq, Ord, Enum, Show) 

type Parameter = Either Variable Int

data Instruction
    = Inp Variable
    | Add Variable Parameter
    | Mul Variable Parameter
    | Div Variable Parameter
    | Mod Variable Parameter
    | Eql Variable Parameter
    deriving (Eq, Show)

type Memory = Map.Map Variable Int

type Constraint = (Int, Int, Int)

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
convert line = getInstruction ins params
    where
        (ins:params) = words line

charToVariable :: Char -> Variable
charToVariable 'x' = X
charToVariable 'y' = Y
charToVariable 'z' = Z
charToVariable 'w' = W

getInstruction :: String -> [String] -> Instruction
getInstruction "inp" [[param]] = Inp (charToVariable param)
getInstruction "add" [[var], param] = Add (charToVariable var) (readParam param)
getInstruction "mul" [[var], param] = Mul (charToVariable var) (readParam param)
getInstruction "div" [[var], param] = Div (charToVariable var) (readParam param)
getInstruction "mod" [[var], param] = Mod (charToVariable var) (readParam param)
getInstruction "eql" [[var], param] = Eql (charToVariable var) (readParam param)

readParam :: String -> Parameter
readParam param = (if head param `elem` "xyzw" then Left (charToVariable (head param)) else Right (read param)) 

startMemory :: Memory
startMemory = Map.fromList (zip [(X)..(W)] [0,0..])

runProgram :: [Instruction] -> Memory -> [Int] -> Memory
runProgram [] memory input = memory
runProgram (Inp var:instructions) memory (x:input) = runProgram instructions (Map.insert var x memory) input
runProgram (Add var param:instructions) memory input = runProgram instructions (Map.insert var (getVarValue memory var + getParamValue memory param) memory) input
runProgram (Mul var param:instructions) memory input = runProgram instructions (Map.insert var (getVarValue memory var * getParamValue memory param) memory) input
runProgram (Div var param:instructions) memory input = runProgram instructions (Map.insert var (getVarValue memory var `div` getParamValue memory param) memory) input
runProgram (Mod var param:instructions) memory input = runProgram instructions (Map.insert var (getVarValue memory var `mod` getParamValue memory param) memory) input
runProgram (Eql var param:instructions) memory input = runProgram instructions (Map.insert var (getVarValue memory var `eql` getParamValue memory param) memory) input

eql :: Int -> Int -> Int
eql a b = fromEnum (a == b)

getVarValue :: Memory -> Variable -> Int
getVarValue memory = (memory Map.!)

getParamValue :: Memory -> Parameter -> Int
getParamValue memory (Left var) = getVarValue memory var
getParamValue _ (Right val) = val

sections :: [Instruction] -> [[Instruction]]
sections [] = []
sections (x:xs) = (x:t) : sections d
    where
        (t,d) = span (not . isInp) xs

isInp :: Instruction -> Bool
isInp (Inp _) = True
isInp _ = False

isDiv :: Instruction -> Bool
isDiv (Div _ _) = True
isDiv _ = False

isPopDiv :: Instruction -> Bool
isPopDiv (Div Z (Right 26)) = True
isPopDiv _ = False

paramFromAdd :: Instruction -> Int
paramFromAdd (Add _ (Right x)) = x

getConstraints :: [[Instruction]] -> Int -> [(Int, Int)] -> [Constraint]
getConstraints [] _ _ = []
getConstraints (sec:secs) idx stack
    | isPop = (firstIdx, idx, toAdd + toAddWhenPop) : getConstraints secs (idx + 1) (tail stack)
    | otherwise = getConstraints secs (idx + 1) ((idx, toAddWhenPush):stack)
    where
        isPop = isPopDiv $ sec !! 4
        (firstIdx, toAdd) = head stack
        toAddWhenPop = paramFromAdd $ sec !! 5
        toAddWhenPush = paramFromAdd $ sec !! 15

solveConstraintsMax :: [Constraint] -> [(Int, Int)]
solveConstraintsMax [] = []
solveConstraintsMax ((idx1, idx2, diff):cs)
    | diff <= 0 = [(idx1,9),(idx2,9+diff)] ++ solveConstraintsMax cs
    | otherwise = [(idx1,9-diff),(idx2,9)] ++ solveConstraintsMax cs

solveConstraintsMin :: [Constraint] -> [(Int, Int)]
solveConstraintsMin [] = []
solveConstraintsMin ((idx1, idx2, diff):cs)
    | diff <= 0 = [(idx1,1-diff),(idx2,1)] ++ solveConstraintsMin cs
    | otherwise = [(idx1,1),(idx2,1+diff)] ++ solveConstraintsMin cs

generateSolutionFromList :: [(Int, Int)] -> Int
generateSolutionFromList = read . concat . map (show . snd) . sort

solve1 :: [ParsedLine] -> Int
solve1 program = generateSolutionFromList $ solveConstraintsMax $ getConstraints secs 0 []
    where
        secs = sections program

solve2 :: [ParsedLine] -> Int
solve2 program = generateSolutionFromList $ solveConstraintsMin $ getConstraints secs 0 []
    where
        secs = sections program
