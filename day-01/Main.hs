module Main where
import Data.List
import System.Environment

readLines :: FilePath -> IO [Int]
readLines = fmap (fmap read) . (fmap lines . readFile)

findPairs :: [Int] -> Int -> [(Int, Int)]
findPairs xs yr = [(x, y) | x <- xs, y <- xs, x + y == yr]

findTriplets :: [Int] -> Int -> [(Int, Int, Int)]
findTriplets xs yr =
    [(x, y, z)
    | x <- xs
    , y <- xs
    , z <- xs
    , x + y + z == yr
    ]

solvePuzzle :: Int -> [Int] -> Int -> [Int]
solvePuzzle 2 xs yr = nub $ map (\(x, y) -> x * y) (findPairs xs yr)
solvePuzzle 3 xs yr = nub $ map (\(x, y, z) -> x * y * z) (findTriplets xs yr)

main :: IO ()
main = do
    xs <- readLines "input/input.txt"
    print $ head (solvePuzzle 2 xs 2020)
    print $ head (solvePuzzle 3 xs 2020)
