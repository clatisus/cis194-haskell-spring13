{-# OPTIONS_GHC -Wall #-}

module Hanoi where

-- Exercise 5
-- Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
type Peg = String
type Move = (Peg, Peg)
-- move n discs from a to b using c as temporary storage
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n <= 0    = []
  | otherwise = hanoi (n - 1) a c b ++
                [(a, b)] ++
                hanoi (n - 1) c b a

-- Exercise 6
-- move n discs from a to b using c and d as temporary storages
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
  | n <= 0    = []
  | otherwise = hanoi4 k a c b d ++
                hanoi (n - k) a b d ++
                hanoi4 k c b a d
                  where k = n + 1 - (round . (sqrt :: Double -> Double) . fromIntegral) (2 * n + 1)

-- Test
test1 :: Int
test1 = length $ hanoi 15 "a" "b" "c" -- should be 2^15 - 1 = 32767
test2 :: Int
test2 = length $ hanoi4 15 "a" "b" "c" "d" -- should be 129

-- References
--   https://en.wikipedia.org/wiki/Tower_of_Hanoi#Frame%E2%80%93Stewart_algorithm
