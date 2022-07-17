{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield as ds) = do
  diceA <- rollDice (min 3 (as - 1))
  diceD <- rollDice (min 2 ds)
  let outcome = zipWith (<=) diceA diceD
  let diedA = length . filter id $ outcome
  let diedD = length outcome - diedA
  return $ Battlefield (as - diedA) (ds - diedD)
    where rollDice :: Int -> Rand StdGen [Int]
          rollDice n = fmap unDV <$> (reverse . sort <$> replicateM n die)

-- λ> evalRandIO $ rollDice 2
-- [3,3]
-- λ> evalRandIO $ rollDice 2
-- [6,4]
-- λ> evalRandIO $ rollDice 2
-- [5,2]
--
-- λ> evalRandIO (battle $ Battlefield 3 5)
-- Battlefield {attackers = 1, defenders = 5}
-- λ> evalRandIO (battle $ Battlefield 3 5)
-- Battlefield {attackers = 1, defenders = 5}
-- λ> evalRandIO (battle $ Battlefield 3 5)
-- Battlefield {attackers = 3, defenders = 3}

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield as ds)
  | as < 2 || ds <= 0 = return bf
  | otherwise         = battle bf >>= invade

-- λ> evalRandIO (invade $ Battlefield 30 50)
-- Battlefield {attackers = 1, defenders = 16}
-- λ> evalRandIO (invade $ Battlefield 50 10)
-- Battlefield {attackers = 46, defenders = 0}

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  results <- replicateM 1000 $ invade bf
  let success = length . filter ((== 0) . defenders) $ results
  return $ fromIntegral success / 1000.0

-- λ> evalRandIO $ successProb $ Battlefield 100 100
-- 0.816
-- λ> evalRandIO $ successProb $ Battlefield 1000 1000
-- 0.999
