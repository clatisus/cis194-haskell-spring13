{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveFoldable #-}

module JoinList where
import Sized (Sized, getSize, size)

-------------------------------------------------------------------------------
-- Exercise 1

-- m is the monoidal annotation type
-- a is the stored value type
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show, Foldable)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-------------------------------------------------------------------------------
-- Exercise 2

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
-- index out of boundary
indexJ n x
  | n < 0 || n >= sizex = Nothing
  where sizex = getSize . size . tag $ x
indexJ _ (Single _ a) = Just a
indexJ n (Append _ l r)
  | n < sizel = indexJ n l
  | otherwise = indexJ (n - sizel) r
  where sizel = getSize . size . tag $ l
indexJ _ _ = undefined

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
-- drop nothing or all
dropJ n x
  | n <= 0     = x
  | n >= sizex = Empty
  where sizex = getSize . size . tag $ x
-- drop something but not all
dropJ n (Append _ l r)
  | n <= sizel = dropJ n l +++ r
  | otherwise  = dropJ (n - sizel) r
  where sizel = getSize . size . tag $ l
dropJ _ _ = undefined

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
-- take nothing or all
takeJ n x
  | n <= 0     = Empty
  | n >= sizex = x
  where sizex = getSize . size . tag $ x
-- take something but not all
takeJ n (Append _ l r)
  | n <= sizel = takeJ n l
  | otherwise  = l +++ takeJ (n - sizel) r
  where sizel = getSize . size . tag $ l
takeJ _ _ = undefined
