{-# LANGUAGE InstanceSigs #-}
module Scrabble where

newtype Score = Point Int
    deriving Show

instance Semigroup Score where
    (<>) :: Score -> Score -> Score
    Point a <> Point b = Point (a+b)

instance Monoid Score where
    mempty = Point 0

score :: Char -> Score
score 'a' = Point 1
score 'b' = Point 3
score 'c' = Point 3
score 'd' = Point 2
score 'e' = Point 1
score 'f' = Point 4
score 'g' = Point 2
score 'h' = Point 4
score 'i' = Point 1
score 'j' = Point 8
score 'k' = Point 5
score 'l' = Point 1
score 'm' = Point 3
score 'n' = Point 1
score 'o' = Point 1
score 'p' = Point 3
score 'q' = Point 10
score 'r' = Point 1
score 's' = Point 1
score 't' = Point 1
score 'u' = Point 1
score 'v' = Point 4
score 'w' = Point 4
score 'x' = Point 8
score 'y' = Point 4
score 'z' = Point 10
score _ = mempty

scoreString :: String -> Score
scoreString = mconcat . map score

halfString :: String -> (String,String)
halfString s = splitAt h s
    where
    h = length s `div` 2