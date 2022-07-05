{-# OPTIONS_GHC -Wall #-}

module Homework where

import Data.List ((\\))

-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum
      . filter even
      . takeWhile (/= 1)
      . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

-- Exercise 2
-- foldTree "ABCDEFGHIJ" =
-- Node 3
--   (Node 2
--     (Node 0 Leaf 'F' Leaf)
--     'I'
--     (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf))
--   'J'
--   (Node 2
--     (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf)
--     'H'
--     (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
   deriving (Show, Eq)
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where insert x Leaf                              = Node 0 Leaf x Leaf
        insert x (Node _ Leaf y Leaf)              = Node 1 (insert x Leaf) y Leaf
        insert x (Node _ l@(Node nl _ _ _) y Leaf) = Node (nl + 1) l y (insert x Leaf)
        insert x (Node _ Leaf y r@(Node nr _ _ _)) = Node (nr + 1) (insert x Leaf) y r
        insert x (Node _ l@(Node nl _ _ _) y r@(Node nr _ _ _))
          | nl < nr   = Node (nr  + 1) l' y r
          | nl > nr   = Node (nl  + 1) l  y r'
          | otherwise = Node (nr' + 1) l  y r'
          where l'@(Node _ _ _ _)   = insert x l
                r'@(Node nr' _ _ _) = insert x r

-- Exercise 3
-- xor [False, True, False] == True
-- xor [False, True, False, False, True] == False
xor :: [Bool] -> Bool
xor = foldr (\x y -> (x || y) && not (x && y)) False
-- xor = foldr (\a b -> if a then not b else b) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a bs -> f a : bs) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse

myFoldlV2 :: (a -> b -> a) -> a -> [b] -> a
myFoldlV2 f base xs = foldr (\b g x -> g (x `f` b)) id xs base
{-
the right side equals to
  (foldr (\b g x -> g (x `f` b)) id xs) base
the lambda's type is
  b -> (a -> a) -> (a -> a)
the same as
  b -> (a -> a) -> a -> a
so the accumulator type for foldr is (a -> a), and it's initial value is id
now let's look into the lambda, it accepts three parameter
  b is bound to the current xs element
  g is bound to previous accumulator function
  x is bound to the input for the accumulator function
so if we simulate the calculation for
  myFoldlV2 f base [x1, x2, x3]
the steps are:
  - b=x3, g=id, x -> id (x `f` x3)
  - b=x2, g=(\x -> id (x `f` x3)), x -> id ((x `f` x2) `f` x3)
  - b=x1, g=(\x -> id ((x `f` x2) `f` f3)), x -> id (((x `f` x1) `f` x2) `f` x3)
finally, we have x = base is called with the above accumulated function (output of foldr)
-}

-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+ 1) . (* 2)) $ [1..n] \\ sieve
   where sieve = filter (<= n)
               . map (\(a, b) -> a + b + 2 * a * b)
               $ cartProd [1..n] [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- References
--   https://wiki.haskell.org/Foldr_Foldl_Foldl'
