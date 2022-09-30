{-# OPTIONS_GHC -Wall  #-}
module LogAnalysis where

import Log
import Data.Bifunctor
import Text.Read (readMaybe)

firstWord :: String -> (String,String)
firstWord [] = ([], [])
firstWord (s : ss)
    | s == ' ' = ([], ss)
    | otherwise = first (s :) (firstWord ss)

parseMessage :: String -> LogMessage
parseMessage s = case parseType s of
    (Nothing, s')    -> Unknown s'
    (Just pT,s')     -> case parseTime s' of
                        (Nothing, _ ) -> Unknown s
                        (Just pTime, s'' ) ->   LogMessage pT pTime s''
    where
    parseType :: String -> (Maybe MessageType, String)
    parseType sT
        | fst splited == "I" = (Just Info, text)
        | fst splited == "W" = (Just Warning, text)
        | fst splited == "E" = (Just (Error (read $ fst text')), snd text')
        | otherwise = (Nothing,sT)
        where
            splited = firstWord sT
            text = snd splited
            text' = firstWord text
    parseTime :: String -> (Maybe TimeStamp, String)
    parseTime sTime
        |   null (snd splited) = (Nothing, s)
        |   otherwise = first readMaybe splited
        where
            splited = firstWord sTime

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)


insert :: LogMessage -> MessageTree -> MessageTree
insert (LogMessage t time s) (Node tree1 (LogMessage t1 time1 s1) tree2)
    | time <= time1 = Node (insert (LogMessage t time s) tree1) (LogMessage t1 time1 s1) tree2
    | time > time1 = Node tree1 (LogMessage t1 time1 s1) (insert (LogMessage t time s) tree2)
insert (LogMessage t time s) Leaf = Node Leaf (LogMessage t time s) Leaf
insert _ tree = tree


build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node t1 l t2) = inOrder t1 ++ (l : inOrder t2)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messes = map g (inOrder $ build (filter err messes))
    where   g :: LogMessage -> String
            g (LogMessage _ _ s) = s
            g (Unknown s) = s
            err :: LogMessage -> Bool
            err (Unknown _) = False
            err (LogMessage (Error i) _ _) = i > 5
            err LogMessage {} = False