module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative
import Control.Monad.State
import Control.Monad
import qualified Data.Map as Map

data Line
    = Command Command
    | Output Output
    deriving (Eq, Show)
data Command
    = Cd String
    | Ls
    deriving (Eq, Show)

data Output
    = FileOutput Int String
    | DirectoryOutput String
    deriving (Eq, Show)

data File = File Int String deriving (Eq, Show)

type Id = [String]
-- size, files, list of children, parent
data Directory = Directory Int [File] [Id] (Maybe Id) deriving (Eq, Show)

type ParsedLine = Line

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

convert :: String -> ParsedLine
convert line
    | firstPart == "$" = Command $ parseCommand (tail splitLine)
    | otherwise = Output $ parseOutput splitLine
    where
        splitLine = splitBy ' ' line
        firstPart = head splitLine

parseCommand :: [String] -> Command
parseCommand ["ls"] = Ls
parseCommand ["cd", destination] = Cd destination
parseCommand _ = error "Invalid command"

parseOutput :: [String] -> Output
parseOutput ["dir", name] = DirectoryOutput name
parseOutput [size, name] = FileOutput (read size) name
parseOutput _ = error "Invalid output"

type DirectoryStructure = Map.Map [String] Directory

initialStructure :: DirectoryStructure
initialStructure = Map.empty

fullStructure :: [Line] -> DirectoryStructure
fullStructure lines = evalState (buildStructure lines) (initialStructure, [])

type BuildingState = (DirectoryStructure, Id)

buildStructure :: [Line] -> State BuildingState DirectoryStructure
buildStructure [] = do
    (structure, _) <- get
    put (structure, [])
    calculateSizes
buildStructure ((Command (Cd "..")):ls) = do
    modify updateStateWithCdBack
    buildStructure ls
buildStructure ((Command (Cd "/")):ls) = do
    modify updateStateWithRoot
    buildStructure ls
buildStructure ((Command (Cd dir)):ls) = do
    modify (updateStateWithDirectory dir)
    buildStructure ls
buildStructure ((Command Ls):ls) = buildStructure ls
buildStructure ((Output (FileOutput size name)):ls) = do
    modify (addFileToState size name)
    buildStructure ls
buildStructure ((Output (DirectoryOutput name)):ls) = do
    modify (addChildToState name)
    buildStructure ls

updateStateWithCdBack :: BuildingState -> BuildingState
updateStateWithCdBack (structure, []) = (structure, [])
updateStateWithCdBack (structure, currentDir) = (structure, tail currentDir)

updateStateWithRoot :: BuildingState -> BuildingState
updateStateWithRoot (structure, _)
    | Map.member [] structure = (structure, [])
    | otherwise = (Map.insert [] (Directory (-1) [] [] Nothing) structure, [])

updateStateWithDirectory :: String -> BuildingState -> BuildingState
updateStateWithDirectory newDir state@(structure, currentDir)
    | Map.member newCurrentDir structure = state
    | otherwise = (Map.insert newCurrentDir (Directory (-1) [] [] (Just currentDir)) structure, newCurrentDir)
    where
        newCurrentDir = newDir:currentDir

addFileToState :: Int -> String -> BuildingState -> BuildingState
addFileToState size name (structure, currentDir) = (newStructure, currentDir)
    where
        Just (Directory dirSize files children parent) = Map.lookup currentDir structure
        newDir = Directory dirSize (File size name:files) children parent
        newStructure = Map.insert currentDir newDir structure

addChildToState :: String -> BuildingState -> BuildingState
addChildToState name (structure, currentDir) = (newStructure, currentDir)
    where
        Just (Directory dirSize files children parent) = Map.lookup currentDir structure
        newDir = Directory dirSize files ((name:currentDir):children) parent
        newStructure = Map.insert currentDir newDir structure

calculateSizes :: State BuildingState DirectoryStructure
calculateSizes = do
    (structure, currentDir) <- get
    let Just (Directory dirSize files children parent) = Map.lookup currentDir structure
    when (dirSize == (-1)) $ do
            foldM_ calculateChildSizes structure children
            modify (\(s, _) -> (s, currentDir))
            modify updateStateWithSize
            return ()
    gets fst

calculateChildSizes :: DirectoryStructure -> Id -> State BuildingState DirectoryStructure
calculateChildSizes childStructure childId = do
    put (childStructure, childId)
    calculateSizes

updateStateWithSize :: BuildingState -> BuildingState
updateStateWithSize (structure, currentDir) = (newStructure, currentDir)
    where
        Just (Directory dirSize files children parent) = Map.lookup currentDir structure
        childrenSizes = sum $ map (\child -> getSize $ fromJust $ Map.lookup child structure) children
        fileSizes = sum $ map (\(File size _) -> size) files
        newDir = Directory (childrenSizes + fileSizes) files children parent
        newStructure = Map.insert currentDir newDir structure

getSize :: Directory -> Int
getSize (Directory size _ _ _) = size

convert2 :: String -> ParsedLine2
convert2 = convert

sizes :: [Line] -> [Int]
sizes = map getSize . Map.elems . fullStructure

solve1 :: [ParsedLine] -> Int
solve1 = sum . filter (<=100000) . sizes

solve2 :: [ParsedLine2] -> Int
solve2 lines = minimum . filter (>=neededToDelete) $ allSizes
    where
        allSizes = sizes lines
        unused = 70000000 - maximum allSizes
        neededToDelete = 30000000 - unused