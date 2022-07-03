{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Text.Read (readMaybe)

-- Exercise 1
isNumeric :: String -> Bool
isNumeric s = case readMaybe s :: Maybe Int of
  Just _ -> True
  _ -> False

-- parseMessage "E 2 562 help help"
--   == LogMessage (Error 2) 562 "help help"
-- parseMessage "I 29 la la la"
--   == LogMessage Info 29 "la la la"
-- parseMessage "This is not in the right format"
--   == Unknown "This is not in the right format"
parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("I":ts:msg)
    | isNumeric ts -> LogMessage Info (read ts) (unwords msg)
  ("W":ts:msg)
    | isNumeric ts -> LogMessage Warning (read ts) (unwords msg)
  ("E":level:ts:msg)
    | isNumeric level && isNumeric ts -> LogMessage (Error (read level)) (read ts) (unwords msg)
  _ -> Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- testParse parse 10 "error.log"

-- Exercise 2
-- left child < node <= right child
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert u@(LogMessage _ ts _) (Node l t@(LogMessage _ cur _) r)
  | ts < cur  = Node (insert u l) t r
  | otherwise = Node l t (insert u r)
insert _ (Node _ (Unknown _) _) = error "Invalid MessageTree"

-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l msg r) = inOrder l ++ [msg] ++ inOrder r

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . filter isSevere . inOrder . build
  where getMessage (LogMessage _ _ msg)     = msg
        getMessage (Unknown _)              = error "Unreachable"
        isSevere (LogMessage (Error x) _ _) = x >= 50
        isSevere _                          = False

-- testWhatWentWrong parse whatWentWrong "sample.log"

-- Exercise 6
-- testWhatWentWrong parse whatWentWrong "error.log"
{-
Mustardwatch opened, please close for proper functioning!
All backup mustardwatches are busy
Depletion of mustard stores detected!
Hard drive failure: insufficient mustard
All backup mustardwatches are busy
Twenty seconds remaining until out-of-mustard condition
Ten seconds remaining until out-of-mustard condition
Empty mustard reservoir! Attempting to recover...
Recovery failed! Initiating shutdown sequence
-}