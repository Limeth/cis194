module Week1.Week1 (validate, hanoi) where

-- Week 1

factorial :: Integer -> Integer
factorial 0 = 1
factorial a = a * factorial (pred a)

hailstone :: Integer -> Integer
hailstone n
    | n `mod` 2 == 0 = n `div` 2
    | otherwise      = 3 * n + 1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

-- Homework 1

-- Part 1: Payment card validation

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse $ toDigitsRev n

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:y:zs) = (x * 2) : y : doubleEveryOther zs

sumDigitsInner :: [Integer] -> Integer
sumDigitsInner [] = 0
sumDigitsInner (x:zs) = x + sumDigitsInner zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:zs) = sumDigitsInner (toDigitsRev x) + sumDigits zs

validate :: Integer -> Bool
validate x = (sumDigits $ doubleEveryOther $ toDigits x) `mod` 10 == 0

-- Part 2: Towers of Hanoi

type Peg = String
type Move = (Peg, Peg)

move :: Integer -> Peg -> Peg -> Peg -> [Move]
move 1 from to temp = [(from, to)]
move n from to temp = move (n - 1) from temp to ++ (from, to) : move (n - 1) temp to from

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c = move n a c b
