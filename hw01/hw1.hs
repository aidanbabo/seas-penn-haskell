{-# OPTIONS_GHC -Wall #-}

-- https://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
doubleEveryOther :: [Integer] -> [Integer]
sumDigits :: [Integer] -> Integer
validate :: Integer -> Bool

toDigitsRev n
    | n <= 0 = []
    | otherwise = mod n 10 : toDigitsRev ( div n 10 )

toDigits n = reverse (toDigitsRev n)

doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:rest) = x : y * 2 : (doubleEveryOther rest)

sumDigits [] = 0
sumDigits (x:xs)
    | x < 10 = x + sumDigits xs
    | otherwise = sumDigits ( (toDigits x) ++ xs )

validate n = mod ( sumDigits $ doubleEveryOther $ toDigitsRev n ) 10 == 0


--------------------------------------
-- Hanoi
--------------------------------------

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

-- s is start, f is finish, t is temp (storage)
-- In the simplest cast, move the one disk from start to finish
hanoi 1 s f _ = [(s, f)]
-- In all other cases, move all but the largest disk from the start to a temporary place.
-- Then move the largest disk to the finish, then put all of them back.
hanoi n s f t = (hanoi (n-1) s t f) ++ [(s, f)] ++ (hanoi (n-1) t f s)

-- TODO hanoi with 4 pegs
