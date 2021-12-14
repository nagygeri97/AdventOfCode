module Main where

import Data.List
import Data.Function
import System.IO

main :: IO ()
main = mainLoop []

mainLoop :: [[Integer]] -> IO ()
mainLoop xs = do
    done <- isEOF
    if done
        then do putStrLn . ("Part One: " ++) . show . solve1 $ xs
                putStrLn . ("Part Two: " ++) . show . solve2 $ xs
        else do input <- pure convert <*> getLine
                mainLoop (xs ++ [input])

convert :: String -> [Integer]
convert = map (fromIntegral . (+(-fromEnum '0')) . fromEnum)

fromBinary :: [Integer] -> Integer
fromBinary = foldl (\acc x -> acc*2 + x) 0

solve1 :: [[Integer]] -> Integer
solve1 input = gamma * epsilon
    where 
        bits = (map . map) (\xs -> (head xs, length xs)) . map group . map sort . transpose $ input
        gamma = fromBinary . map fst . map (maximumBy (compare `on` snd)) $ bits
        epsilon = fromBinary . map fst . map (minimumBy (compare `on` snd)) $ bits

solve2 :: [[Integer]] -> Integer
solve2 xs = oxygen * co2
    where
        oxygen = fromBinary $ filterBin [] xs compare
        co2 = fromBinary $ filterBin [] xs (flip compare)


filterBin :: [Integer] -> [[Integer]] -> (Integer -> Integer -> Ordering) -> [Integer]
filterBin result [xs] f = result ++ xs
filterBin result list f = filterBin (result ++ [keep]) (map tail (filter ((==keep) . head) list)) f
    where 
        keep = fst . maximumBy (f `on` fromIntegral . snd) . map (\xs -> (head xs, length xs)) . group . sortBy f . map head $ list 