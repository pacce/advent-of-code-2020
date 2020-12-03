module Main where
import Control.Monad

readLines :: FilePath -> IO [Int]
readLines = fmap rd . readFile
        where rd x = map (read :: String -> Int) (lines x)

main :: IO ()
main = putStrLn "Hello, Haskell!"
