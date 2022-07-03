{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.List (transpose)

-- Exercise 1 Hopscotch
-- skips "ABCD"       == ["ABCD","BD","C","D"]
-- skips "hello!"     == ["hello!","el!","l!","l","o","!"]
-- skips [1]          == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips []           == []
skips :: [a] -> [[a]]
skips xs = [[xs !! (j - 1) | j <- [i,i + i..length xs]] | i <- [1..length xs]]

-- Exercise 2 Local maxima
-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | x < y && y > z = y:localMaxima (z:zs)
  | otherwise      = localMaxima (y:z:zs)
localMaxima _ = []
-- localMaxima xs = [xs !! (i - 1) | i <- [2..length xs - 1], xs !! (i - 2) < xs !! (i - 1) && xs !! (i - 1) > xs !! i]

-- Exercise 3 Histogram
-- putStr (histogram [1,1,1,5]) ==
--  *
--  *
--  *   *
-- ==========
-- 0123456789
-- putStr (histogram [1,4,5,4,6,6,3,4,2,4,9]) ==
--     *
--     *
--     * *
--  ******  *
-- ==========
-- 0123456789
-- construct rows
histogram :: [Integer] -> String
histogram xs = unlines $ [line i | i <- [m,m - 1..1]] ++ ["==========\n0123456789"]
  where cnt    = [length $ filter (i ==) xs | i <- [0..9]]
        m      = maximum cnt
        line n = [if c >= n then '*' else ' ' | c <- cnt]
-- another method is to construct columns then transpose
histogramV2 :: [Integer] -> String
histogramV2 xs = unlines $ transpose $ [column n c | (n, c) <- zip ([0..9]::[Int]) cnt]
  where cnt        = [length $ filter (i ==) xs | i <- [0..9]]
        m          = maximum cnt
        column n c = replicate (m - c) ' ' ++ replicate c '*' ++ "=" ++ show n
