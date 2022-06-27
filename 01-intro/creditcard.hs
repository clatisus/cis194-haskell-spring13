--
-- Example: toDigits 1234 == [1,2,3,4]
-- Example: toDigitsRev 1234 == [4,3,2,1]
-- Example: toDigits 0 == []
-- Example: toDigits (-17) == []
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0 = (n `mod` 10) : toDigitsRev (n `div` 10)
  | otherwise = []

--
-- Example: doubleEveryOther [8, 7, 6, 5] == [16, 7, 12, 5]
-- Example: doubleEveryOther [1, 2, 3] == [1, 4, 3]
-- Example: doubleEveryOther [1, 2, 3, 4] == [2, 2, 6, 4]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) oneTwo . reverse
  where oneTwo = 1 : 2 : oneTwo

--
-- Example: sumDigits [16, 7, 12, 5] == 1 + 6 + 7 + 1 + 2 + 5 == 22
sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

--
-- Example: validate 4012888888881881 == True
-- Example: validate 4012888888881882 == False
-- Example: validate (-4012888888881882) == False
validate :: Integer -> Bool
validate x
  | x > 0     = (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0
  | otherwise = False

-- Alternatives
doubleEveryOtherV2 :: [Integer] -> [Integer]
doubleEveryOtherV2 xs = reverse $ map snd $ map multiplyOddPosition $ zip [0..] $ reverse xs

doubleEveryOtherV3 :: [Integer] -> [Integer]
doubleEveryOtherV3 = reverse . map snd . map multiplyOddPosition . zip [0..] . reverse

multiplyOddPosition :: (Integer, Integer) -> (Integer, Integer)
multiplyOddPosition (x, y)
  | odd x     = (x, 2 * y)
  | otherwise = (x, y)

sumDigitsV2 :: [Integer] -> Integer
sumDigitsV2 = sum . map (sum . toDigits)

-- References
--   https://typeclasses.com/featured/dollar
--   https://hackage.haskell.org/package/base-4.16.1.0/docs/GHC-List.html
