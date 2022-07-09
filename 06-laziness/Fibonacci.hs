{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

-------------------------------------------------------------------------------
-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- O(F_n)
fibs1 :: [Integer]
fibs1 = [fib n | n <- [0..]]

-------------------------------------------------------------------------------
-- Exercise 2

-- O(n)
fibs2 :: [Integer]
fibs2 = 0:1:zipWith (+) fibs2 (tail fibs2)

fibs2V2 :: [Integer]
fibs2V2 = f 0 1
  where f a b = a:f b (a + b)

-------------------------------------------------------------------------------
-- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a as) = a:streamToList as

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

testEx3 :: Stream Integer
testEx3 = listToStream [0..]
  where listToStream [] = undefined
        listToStream (a:as) = Cons a (listToStream as)
-- λ> testEx3
-- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]

-------------------------------------------------------------------------------
-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)
-- λ> streamRepeat 1
-- [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a as) = Cons (f a) (streamMap f as)
-- λ> streamMap (+2) testEx3
-- [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21]

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))
-- λ> streamFromSeed (+2) 0
-- [0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38]

-------------------------------------------------------------------------------
-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) bs = Cons a (interleaveStreams bs as)
-- the following is wrong, it will cause `ruler` to stack overflow.
-- because `ruler'` is recursively defined, just looking up for the
-- head `a` will try to deconstruct `b` which result in another recursion
-- interleaveStreams (Cons a as) (Cons b bs) = Cons a (Cons b (interleaveStreams as bs))

-- https://oeis.org/A007814
-- https://oeis.org/A001511
-- (A007814) = interleaveStreams (streamRepeat 0) (A001511)
-- (A001511) = streamMap (+1) (A007814)
ruler :: Stream Integer
ruler = ruler' 0

ruler' :: Integer -> Stream Integer
ruler' a = interleaveStreams (streamRepeat a) (ruler' (a + 1))

-------------------------------------------------------------------------------
-- Exercise 6

-- x = 0 + 1x + 0x^2 + 0x^3 + ...
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#extension-FlexibleInstances
instance Num (Stream Integer) where
   fromInteger n = Cons n (streamRepeat 0)
   negate ns = streamMap (0-) ns
   (+) (Cons a as) (Cons b bs) = Cons (a + b) (as + bs)
   -- AB = (a0 + xA′)B
   --    = a0B + xA′B
   --    = a0(b0 + xB′) + xA′B
   --    = a0b0 + x(a0B′ + A′B)
   (*) (Cons a0 as) b@(Cons b0 bs) = Cons (a0 * b0) (streamMap (*a0) bs + as * b)

-- λ> x^4
-- [0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
-- λ> (1 + x)^5
-- [1,5,10,10,5,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
-- λ> (x^2 + x + 3) * (x - 5)
-- [-15,-2,-4,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

instance Fractional (Stream Integer) where
  -- if A=a0+xA′ and B=b0+xB′,then A/B=Q, where
  -- Q = (a0/b0) + x((1/b0)(A′ − QB′)).
  -- not work for all cases (like when b0=0), but is only used in the valid cases
  (/) (Cons a0 as) (Cons b0 bs) = q
    where q = Cons (div a0 b0) (streamMap (`div` b0) (as - q * bs))

-- F(x) = F_0 + F_1 x + F_2 x^2 + F_3 x^3 + ...
-- x + xF(x) + x^2F(x) = F(x)
-- F(x) = x / (1 - x - x^2)
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^(2 :: Integer))

-------------------------------------------------------------------------------
-- Exercise 7

data Matrix = Matrix Integer Integer Integer Integer

instance Show Matrix where
  show (Matrix a00 a01 a10 a11) = "[" ++ show a00 ++ ", " ++ show a01 ++ ",\n" ++
                                  " " ++ show a10 ++ ", " ++ show a11 ++ "]\n"

instance Num Matrix where
  (*) (Matrix a00 a01 a10 a11)
      (Matrix b00 b01 b10 b11) = Matrix (a00 * b00 + a01 * b10)
                                        (a00 * b01 + a01 * b11)
                                        (a10 * b00 + a11 * b10)
                                        (a10 * b01 + a11 * b11)

-- F = ( 1 1 )
--     ( 1 0 )
-- then
-- F^n = (F_(n+1) F_n    )
--       (F_n     F_(n-1))
-- O(log n)
fib4 :: Integer -> Integer
fib4 n = a11
  where (Matrix _ _ _ a11) = f^(n + 1)
        f = Matrix 1 1 1 0
