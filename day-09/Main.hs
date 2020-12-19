module Main where
import Data.Maybe (isJust)

type Value  = Int
type Window = [Int]

chunk :: Int -> [Value] -> [(Value, Window)]
chunk k ys = filter (\(_, v) -> length v == k) (ck k ys)
    where ck :: Int -> [Value] -> [(Value, Window)]
          ck k [] = []
          ck k (x:xs) = [(x, take k xs)] ++ ck k xs

eval :: (Value, Window) -> Maybe Value
eval (x, ws)
    | null ys       = Just x
    | otherwise     = Nothing
    where ys = [w1 + w2 | w1 <- ws, w2 <- ws, w1 + w2 == x, w1 /= w2]

search :: Int -> [Value] -> Window
search v vs = head $ filter (\y -> (foldr (+) 0 y) == v) ys
    where xs = reverse [2..length vs]
          ys = concat $ map (\x -> map snd $ chunk x vs) xs

solve :: Int -> [Value] -> Maybe Value
solve k xs = head $ filter isJust ys
    where ys = map eval (chunk k $ reverse xs)

solve2 :: Int -> [Value] -> Value
solve2 v vs = (maximum xs) + (minimum xs)
    where xs = search v vs

main :: IO ()
main = do
    xs <- fmap lines $ readFile "input/reference.txt"
    let ys = fmap (read::String -> Int) xs

    case solve 5 ys of
        Just x  -> print x
                >> (print $ solve2 x ys)
        Nothing -> print "unsovable"
