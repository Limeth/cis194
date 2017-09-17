module Week3.Week3 where

import Data.List
import Data.Maybe
import Safe

data List t = E | C t (List t)

lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)


-- Homework 3

-- Ex. 1

skips'' :: [a] -> [a] -> Int -> [a]
skips'' (x:xs) rs droplen
  | (length (x:xs)) >= droplen = skips'' (drop droplen xs) (x:rs) droplen
  | otherwise = x:rs
skips'' xs rs droplen = rs

skips' :: [a] -> Int -> [a]
skips' xs i = reverse $ skips'' (drop i xs) [] i

skips :: [a] -> [[a]]
skips xs = map (\i -> skips' xs i) [0..(length xs) - 1]

-- Ex. 2

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [a] = []
localMaxima [a, b] = []
localMaxima (a:b:c:rest)
  | b > a && b > c = b : localMaxima (b:c:rest)
  | otherwise = localMaxima (b:c:rest)

-- Ex. 3

countOccurrences :: [Integer] -> [Integer]
countOccurrences xs = map (\x -> toInteger $ length (elemIndices x xs)) [0..9]

buildLine :: [Integer] -> Integer -> Integer -> String
buildLine xs line maxOccurrence = map (\(i, x) -> if (x + line - maxOccurrence >= 0) then '*' else ' ') (zip [0..] xs)

histogram :: [Integer] -> String
histogram xs = do
  let occurrences = countOccurrences xs
  let maxOccurrence = maximum occurrences
  unlines $ map (\l -> buildLine occurrences l maxOccurrence) [0..(maxOccurrence - 1)]
  ++ "==========\n0123456789\n"
