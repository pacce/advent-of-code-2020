module Main where
import Data.List

readLines :: FilePath -> IO [Int]
readLines = fmap (fmap read) . (fmap lines . readFile)

findPairs :: [Int] -> Int -> [(Int, Int)]
findPairs xs yr = [(x, y) | x <- xs, y <- xs, x + y == yr]

solvePuzzle :: [Int] -> Int -> [Int]
solvePuzzle xs yr = nub $ map (\(x, y) -> x * y) (findPairs xs yr)

main :: IO ()
main = putStrLn "Hello, Haskell!"
