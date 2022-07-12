{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleInstances #-}

module Scrabble where
import Data.Char (toLower)
import JoinList
    ( (+++), dropJ, indexJ, tag, takeJ, JoinList(Empty, Single) )
import Buffer (Buffer(..))
import Sized (Size (Size), getSize)
import Editor (runEditor, editor)

-------------------------------------------------------------------------------
-- Exercise 3

newtype Score = Score Int
  deriving Show

getScore :: Score -> Int
getScore (Score i) = i

instance Semigroup Score where
  (<>) (Score a) (Score b) = Score (a + b)

instance Monoid Score where
  mempty = Score 0

score :: Char -> Score
score c
  | c' `elem` "aeilnorstu" = Score 1
  | c' `elem` "dg"         = Score 2
  | c' `elem` "bcmp"       = Score 3
  | c' `elem` "fhvwy"      = Score 4
  | c' `elem` "k"          = Score 5
  | c' `elem` "jx"         = Score 8
  | c' `elem` "qz"         = Score 10
  | otherwise              = Score 0
  where c' = toLower c

scoreString :: String -> Score
scoreString = mconcat . map score

scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x

-------------------------------------------------------------------------------
-- Exercise 4

toSingle :: String -> JoinList (Score, Size) String
toSingle x = Single (scoreString x, Size 1) x

instance Buffer (JoinList (Score, Size) String) where
  toString = foldMap (++ "\n")
  fromString = foldt (+++) Empty . map toSingle . lines
    -- https://wiki.haskell.org/Fold#Tree-like_folds
    where foldt :: (a -> a -> a) -> a -> [a] -> a
          foldt _ z []  = z
          foldt _ _ [x] = x
          foldt f z xs  = foldt f z (pairs f xs)
          pairs :: (a -> a -> a) -> [a] -> [a]
          pairs f (x:y:t) = f x y : pairs f t
          pairs _ t       = t
  line = indexJ
  replaceLine n l b = uncurry replaceLine' (takeJ n b, dropJ (n + 1) b)
    where replaceLine' pre Empty = pre
          replaceLine' pre lst   = pre +++ toSingle l +++ lst
  numLines = getSize . snd . tag
  value = getScore . fst . tag

welcomeMsg :: JoinList (Score, Size) String
welcomeMsg = fromString . unlines $
               [ "This buffer is for notes you don't want to save, and for"
               , "evaluation of steam valve coefficients."
               , "To load a different file, type the character L followed"
               , "by the name of the file."
               ]

main :: IO()
main = runEditor editor welcomeMsg
