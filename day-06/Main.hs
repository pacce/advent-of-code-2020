{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Set
import Text.Parsec
import Text.Parsec.Text

type Person = [Char]
type Group  = [Person]

person :: Stream s m Char => ParsecT s u m Person
person = (many1 letter) <* newline

group :: Stream s m Char => ParsecT s u m [Group]
group = sepBy (many1 person) newline

process :: Group -> Int
process = length . unions . fmap Data.Set.fromList

solve :: [Group] -> Int
solve gs = Prelude.foldr (+) 0 (fmap process gs)

main :: IO ()
main = do
    { result <- parseFromFile group "input/input.txt"
    ; case result of
        Left err -> print err
        Right ps -> print $ solve ps
    }
