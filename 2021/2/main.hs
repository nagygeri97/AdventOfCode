module Main where

import Data.List
import System.IO

data Move
    = Forward {len :: Integer}
    | Up {len :: Integer}
    | Down {len :: Integer}
    deriving (Eq, Show)

main :: IO ()
main = mainLoop []

mainLoop :: [Move] -> IO ()
mainLoop xs = do
    done <- isEOF
    if done
        then do putStrLn . ("Part One: " ++) . show . solve1 $ xs
                putStrLn . ("Part Two: " ++) . show . solve2 $ xs
        else do input <- pure createMove <*> getLine
                mainLoop (xs ++ [input])

createMove :: String -> Move
createMove s = case dir of
    "forward" -> Forward
    "up" -> Up
    "down" -> Down
    $ len
    where 
        [dir, lenStr] = words s
        len = read lenStr :: Integer

solve1 :: [Move] -> Integer
solve1  = uncurry (*) . foldl (\(x,y) move -> case move of
    Forward len -> (x+len, y)
    Up len -> (x,y-len)
    Down len -> (x, y+len)) (0,0)

solve2 :: [Move] -> Integer
solve2 = uncurry (*) . fst . foldl (\((x,y),aim) move -> case move of
    Forward len -> ((x+len, y+aim*len), aim)
    Up len -> ((x,y), aim-len)
    Down len -> ((x, y), aim+len)) ((0,0),0)
