{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT ( ExprT(..) )
import Parser ( parseExp )
import StackVM
    ( stackVM, StackVal, StackExp(Mul, PushI, Add), Program )
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

-------------------------------------------------------------------------------
-- Exercise 1
-- eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20
eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

-------------------------------------------------------------------------------
-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

-------------------------------------------------------------------------------
-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-------------------------------------------------------------------------------
-- Exercise 4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax  = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7    = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit = Mod7
  add (Mod7 x) (Mod7 y) = Mod7 . (`mod` 7) $ x + y
  mul (Mod7 x) (Mod7 y) = Mod7 . (`mod` 7) $ x * y

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger  = testExp

testBool :: Maybe Bool
testBool = testExp

testMM :: Maybe MinMax
testMM = testExp

testSat :: Maybe Mod7
testSat = testExp

-------------------------------------------------------------------------------
-- Exercise 5
instance Expr Program where
  lit x = [PushI x]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

run :: String -> Either String StackVal
run = stackVM . fromMaybe [] . compile

-------------------------------------------------------------------------------
-- Exercise 6
class HasVars a where
  var :: String -> a

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

callOnMaybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
callOnMaybe _ Nothing _ = Nothing
callOnMaybe _ _ Nothing = Nothing
callOnMaybe f (Just a) (Just b) = Just $ f a b

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x _ = Just x
  add x y m = callOnMaybe (+) (x m) (y m)
  mul x y m = callOnMaybe (*) (x m) (y m)

withVars :: [(String, Integer)]
          -> (M.Map String Integer -> Maybe Integer)
          -> Maybe Integer
withVars vs e = e $ M.fromList vs

-- λ> :t add (lit 3) (var "x")
-- add (lit 3) (var "x") :: (Expr a, HasVars a) => a
-- λ> withVars [("x", 6)] $ add (lit 3) (var "x")
-- Just 9
-- λ> withVars [("x", 6)] $ add (lit 3) (var "y")
-- Nothing
-- λ> withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
-- Just 54
