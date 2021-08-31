{-# OPTIONS_GHC -Wall #-}

import Log

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

parseMessage :: String -> LogMessage

parseMessage (c:_:cs) 
    | c == 'I' = (infoParse $ words cs)
    | c == 'W' = (warningParse $ words cs)
    | c == 'E' = (errorParse $ words cs)
parseMessage s = Unknown s

infoParse :: [String] -> LogMessage
warningParse :: [String] -> LogMessage
errorParse :: [String] -> LogMessage

infoParse (x:r) = LogMessage Info (read x) (unwords r)
infoParse s = Unknown $ unwords s
warningParse (x:r) = LogMessage Warning (read x) (unwords r)
warningParse s = Unknown $ unwords s
errorParse (x:y:r) = LogMessage (Error (read x)) (read y) (unwords r)
errorParse s = Unknown $ unwords s

---------------------
-- The Tree
---------------------

sorted :: String -> [LogMessage]
sorted s = inOrder . build $ parse s

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = inOrder left ++ [lm] ++ inOrder right

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (lm:lms) = insert lm (build lms)

insert :: LogMessage -> MessageTree -> MessageTree
insert lm@(LogMessage _ _ _) Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ time _) (Node left (LogMessage _ treeTime _) right)
    | time > treeTime = insert lm right
    | time <= treeTime = insert lm left
-- Covers Unkown
insert _ ms = ms    
