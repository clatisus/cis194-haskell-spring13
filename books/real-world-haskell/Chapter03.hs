{-# OPTIONS_GHC -Wall #-}

module Exercise where

import Data.List (sortBy, delete, minimumBy)

-- Exercise 1
data List a = Nil | Cons a (List a)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

-- Exercise 2
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a)) deriving (Show)

tree :: Tree Int
tree = Node 1 (Just (Node 2 Nothing (Just (Node 3 Nothing Nothing)))) Nothing

-- Exercise 1, 2
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Exercise 3
mean :: (Fractional a) => [a] -> a
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

-- Exercise 4
palindrome :: [a] -> [a]
palindrome [] = []
palindrome (x:xs) = [x] ++ (palindrome xs) ++ [x]

-- Exercise 5
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = (x == last xs) && isPalindrome (init xs)

-- Exercise 6
sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (\x y -> compare (length x) (length y))

-- Exercise 7
intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ [xs] = xs
intersperse x (xs:xss) = xs ++ [x] ++ intersperse x xss

-- Exercise 8
height :: Maybe (Tree a) -> Int
height Nothing = 0
height (Just (Node _ l r)) = max (height l) (height r) + 1

-- Exercise 9, 10
data Point t = Point t t deriving (Eq, Show)
data Direction = MyLeft | MyRight | MyStraight deriving (Show)
getDirection :: (Num a, Ord a) => (Point a) -> (Point a) -> (Point a) -> Direction
getDirection (Point ax ay) (Point bx by) (Point cx cy)
  | dp > 0    = MyLeft
  | dp < 0    = MyRight
  | otherwise = MyStraight
  where dp = ((ay - by) * (cx - bx)) + ((bx - ax) * (cy - by))

-- Exercise 11
getDirectionBatch :: (Num a, Ord a) => [Point a] -> [Direction]
getDirectionBatch [] = []
getDirectionBatch [_] = []
getDirectionBatch [_, _] = []
getDirectionBatch (x:y:z:xs) = (getDirection x y z) : getDirectionBatch (y:z:xs)

-- Exercise 12
-- Convex hull using Graham Scan algorithm
sub :: (Point Double) -> (Point Double) -> Point Double
sub (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

cross :: (Point Double) -> (Point Double) -> Double
cross (Point ax ay) (Point bx by) = ax * by - ay * bx

dist :: (Point Double) -> (Point Double) -> Double
dist (Point ax ay) (Point bx by) = sqrt ((ax - bx) ^ (2 :: Int) + (ay - by) ^ (2 :: Int))

cmpCross :: (Point Double) -> (Point Double) -> (Point Double) -> Ordering
cmpCross p a b = let cmp  = compare (cross (b `sub` p) (a `sub` p)) 0.0
                 in if cmp == EQ
                    then compare (dist p a) (dist p b)
                    else cmp

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub [a] = [a]
nub (x:y:xs)
  | x == y = nub (y:xs)
  | otherwise = x : nub (y:xs)

grahamScan :: [Point Double] -> [Point Double]
grahamScan ps = let pivot    = minimumBy (\(Point ax ay) (Point bx by) ->
                                             let cmp = compare ay by
                                             in if cmp == EQ then compare ax bx else cmp) ps
                    sortedPs = delete pivot . nub . sortBy (cmpCross pivot) $ ps
                in grahamScanImpl (tail sortedPs) [head sortedPs, pivot]

-- remaining points -> stack points -> convex hull
grahamScanImpl :: [Point Double] -> [Point Double] -> [Point Double]
grahamScanImpl [] stack = stack
grahamScanImpl (b:ps) [p] = grahamScanImpl ps [b, p]
grahamScanImpl (b:ps) (p:a:stack) = let cmp = compare (cross (p `sub` a) (b `sub` p)) 0.0
                                    in if cmp == GT
                                       then grahamScanImpl ps (b:p:a:stack)
                                       else grahamScanImpl (b:ps) (a:stack)
grahamScanImpl _ _ = error "grahamScanImpl: stack is empty"

{-
Test 1:
grahamScan
[Point 0.3215348546593775 0.03629583077160248,
Point 0.02402358131857918 (-0.2356728797179394),
Point 0.04590851212470659 (-0.4156409924995536),
Point 0.3218384001607433 0.1379850698988746,
Point 0.11506479756447 (-0.1059521474930943),
Point 0.2622539999543261 (-0.29702873322836),
Point (-0.161920957418085) (-0.4055339716426413),
Point 0.1905378631228002 0.3698601009043493,
Point 0.2387090918968516 (-0.01629827079949742),
Point 0.07495888748668034 (-0.1659825110491202),
Point 0.3319341836794598 (-0.1821814101954749),
Point 0.07703635755650362 (-0.2499430638271785),
Point 0.2069242999022122 (-0.2232970760420869),
Point 0.04604079532068295 (-0.1923573186549892),
Point 0.05054295812784038 0.4754929463150845,
Point (-0.3900589168910486) 0.2797829520700341,
Point 0.3120693385713448 (-0.0506329867529059),
Point 0.01138812723698857 0.4002504701728471,
Point 0.009645149586391732 0.1060251100976254,
Point (-0.03597933197019559) 0.2953639456959105,
Point 0.1818290866742182 0.001454397571696298,
Point 0.444056063372694 0.2502497166863175,
Point (-0.05301752458607545) (-0.06553921621808712),
Point 0.4823896228171788 (-0.4776170002088109),
Point (-0.3089226845734964) (-0.06356112199235814),
Point (-0.271780741188471) 0.1810810595574612,
Point 0.4293626522918815 0.2980897964891882,
Point (-0.004796652127799228) 0.382663812844701,
Point 0.430695573269106 (-0.2995073500084759),
Point 0.1799668387323309 (-0.2973467472915973),
Point 0.4932166845474547 0.4928094162538735,
Point (-0.3521487911717489) 0.4352656197131292,
Point (-0.4907368011686362) 0.1865826865533206,
Point (-0.1047924716070224) (-0.247073392148198),
Point 0.4374961861758457 (-0.001606279519951237),
Point 0.003256207800708899 (-0.2729194320486108),
Point 0.04310378203457577 0.4452604050238248,
Point 0.4916198379282093 (-0.345391701297268),
Point 0.001675087028811806 0.1531837672490476,
Point (-0.4404289572876217) (-0.2894855991839297)]
Answer:
[Point (-0.161920957418085) (-0.4055339716426413),Point (-0.4404289572876217) (-0.2894855991839297),Point (-0.4907368011686362) 0.1865826865533206,Point (-0.3521487911717489) 0.4352656197131292,Point 5.054295812784038e-2 0.4754929463150845,Point 0.4932166845474547 0.4928094162538735,Point 0.4916198379282093 (-0.345391701297268),Point 0.4823896228171788 (-0.4776170002088109)]

Test 2:
grahamScan [Point (-3) 1,Point (-4) 1,Point (-1) 4,Point 0 0,Point 2 2,Point (-1) 3,Point (-1) 2,Point 1 0,Point 3 (-1),Point (-1) (-1)]
Answer:
[Point (-4.0) 1.0,Point (-1.0) 4.0,Point 2.0 2.0,Point 3.0 (-1.0),Point (-1.0) (-1.0)]
-}
