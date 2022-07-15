{- CIS 194 HW 10
   due Monday, 1 April
-}
{-# OPTIONS_GHC -Wall #-}

module AParser where

import           Data.Char
import Control.Applicative (Alternative(..))

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-------------------------------------------------------------------------------
-- Exercise 1

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f (Parser r) = Parser r'
    where r' x = first f <$> r x

--  fmap f (Parser r) = Parser (fmap (first f) . r)
-- first f :: (a, c) -> (b, c)
-- fmap (first f) :: Maybe (a, c) -> Maybe (b, c)

-------------------------------------------------------------------------------
-- Exercise 2

instance Applicative Parser where
  pure x = Parser $ Just . (,) x
  p1 <*> p2 = Parser p
    where p x = case runParser p1 x of
            Nothing -> Nothing
            Just (p1', rest) -> runParser (fmap p1' p2) rest

-------------------------------------------------------------------------------
-- Exercise 3

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'
-- λ> runParser abParser "abcdef"
-- Just (('a','b'),"cdef")
-- λ> runParser abParser "aebcdf"
-- Nothing

abParser_ :: Parser ()
-- abParser_ = () <$ char 'a' <* char 'b'
abParser_ = () <$ abParser

-- λ> runParser abParser_ "abcdef"
-- Just ((),"cdef")
-- λ> runParser abParser_ "aebcdf"
-- Nothing

intPair :: Parser [Integer]
-- intPair = (:) <$> posInt <* char ' ' <*> ((:[]) <$> posInt)
intPair = (\a _ b -> [a, b]) <$> posInt <*> char ' ' <*> posInt

-- λ> runParser intPair "12 34"
-- Just ([12,34],"")

-------------------------------------------------------------------------------
-- Exercise 4

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser rp1) <|> (Parser rp2)= Parser $ \x -> rp1 x <|> rp2 x

-------------------------------------------------------------------------------
-- Exercise 5

intOrUpperCase :: Parser ()
intOrUpperCase = (() <$ posInt) <|> (() <$ satisfy isUpper)

-- λ> runParser intOrUpperCase "342abcd"
-- Just ((),"abcd")
-- λ> runParser intOrUpperCase "XYZ"
-- Just ((),"YZ")
-- λ> runParser intOrUpperCase "foo"
-- Nothing
