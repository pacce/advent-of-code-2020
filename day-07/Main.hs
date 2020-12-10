{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.List
import Text.Parsec
import Text.Parsec.Text

type Adjective      = [Char]
type Bag            = [Char]
type Color          = [Char]
type Description    = [Char]
type Rule           = (Description, [Description])
type Verb           = [Char]

adjective :: Stream s m Char => ParsecT s u m Adjective
adjective = many1 letter

bag :: Stream s m Char => ParsecT s u m Bag
bag = string "bag" <* optional (char 's')

color :: Stream s m Char => ParsecT s u m Color
color = many1 letter

natural :: Stream s m Char => ParsecT s u m Int
natural = read <$> many1 digit

verb :: Stream s m Char => ParsecT s u m Verb
verb = string "contain"

description :: Stream s m Char => ParsecT s u m Description
description = do
    { a <- adjective
    ; _ <- spaces
    ; c <- color
    ; _ <- spaces
    ; _ <- bag
    ; return (a ++ " " ++ c)
    }

empty :: Stream s m Char => ParsecT s u m [Description]
empty = do
    { _ <- string "no other"
    ; _ <- spaces
    ; _ <- bag
    ; return []
    }

container :: Stream s m Char => ParsecT s u m Description
container = description

contained :: Stream s m Char => ParsecT s u m [Description]
contained = choice
    [ do { _ <- natural ; _ <- spaces ; x <- description ; return [x] }
    , empty
    ]

rule :: Stream s m Char => ParsecT s u m Rule
rule = do
    { x     <- container
    ; _     <- spaces
    ; _     <- verb
    ; _     <- spaces
    ; ys    <- sepBy contained (string ", ")
    ; _     <- char '.'
    ; _     <- endOfLine
    ; return (x, foldr (++) [] ys)
    }

valid :: [Rule] -> Description -> [Description]
valid rs d = map fst (filter (\(x, y) -> d `elem` y) rs)

solve :: [Rule] -> Description -> [Description]
solve rs d = xs `union` (foldr (++) [] ys)
    where xs = valid rs d
          ys = map (solve rs) xs

main :: IO ()
main = do
    { result <- parseFromFile (many1 rule) "input/reference.txt"
    ; case result of
        Left err    -> print err
        Right rules -> print $ length (solve rules "shiny gold")
    }
