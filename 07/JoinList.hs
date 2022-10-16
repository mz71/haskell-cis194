{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module JoinList where
import Sized
import Scrabble
import Buffer
import Data.Monoid
import Editor

{-data JoinListBasic a = Empty
                        | Single a
                        | Append (JoinListBasic a) (JoinListBasic a)
-}

data JoinList m a = Empty
                | Single m a
                | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)


-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m1 jl1 jl2) = m1



(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

leaf :: Monoid m => a -> JoinList m a
leaf = Single mempty

-- Exercise 2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

tagSize :: (Sized b, Monoid b) => JoinList b a -> Int
tagSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) =>
        Int -> JoinList b a -> Maybe a
indexJ i Empty = Nothing
indexJ 0 (Single m a) = Just a
indexJ ine0 (Single m a) = Nothing
indexJ i (Append siz jl1 jl2)
    | i < tagSize jl1 = indexJ i jl1
    | otherwise = indexJ (i - tagSize jl1) jl2

dropJ :: (Sized b, Monoid b) =>
        Int -> JoinList b a -> JoinList b a
dropJ i Empty = Empty
dropJ i jls@(Single _ _)
    | i <= 0 = jls
    | otherwise = Empty
dropJ i jl@(Append siz jl1 jl2)
    | i <= 0 = jl
    | i < tagSize jl1 = dropJ i jl1 +++ jl2
    | otherwise = Empty +++ dropJ (i - tagSize jl1) jl2


takeJ :: (Sized b, Monoid b) =>
        Int -> JoinList b a -> JoinList b a
takeJ i Empty = Empty
takeJ i jls@(Single _ _)
    | i <= 0 = Empty
    | otherwise = jls
takeJ i jl@(Append siz jl1 jl2)
    | i <= 0 = Empty
    | i < tagSize jl1 = takeJ i jl1
    | otherwise = jl1 +++ takeJ (i - tagSize jl1) jl2

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

fromLines :: [String] -> JoinList (Score, Size) String
fromLines [] = Empty
fromLines [str] = Single (scoreString str, 1) str
fromLines (str : strs)  = fromLines [str] +++ fromLines strs

instance Buffer (JoinList (Score, Size) String) where
  toString :: JoinList (Score, Size) String -> String
  toString = unlines . jlToList
  fromString :: String -> JoinList (Score, Size) String
  fromString = fromLines . lines
  line :: Int -> JoinList (Score, Size) String -> Maybe String
  line = indexJ
  replaceLine :: Int
    -> String
    -> JoinList (Score, Size) String
    -> JoinList (Score, Size) String
  replaceLine i s Empty = Empty
  replaceLine 0 s (Single _ _) = Single (scoreString s, 1) s
  replaceLine i s t@(Single _ _) = t
  replaceLine i s (Append (x,Size j) jl1 jl2)
    | i < getSize (snd (tag jl1)) = Append (x, Size j) (replaceLine i s jl1) jl2
    | otherwise = Append (x, Size j) jl1 (replaceLine (i - getSize (snd (tag jl1))) s jl2)
  numLines :: JoinList (Score, Size) String -> Int
  numLines (Single _ _) = 1
  numLines Empty = 0
  numLines (Append (_, Size i) jl1 jl2) = i
  value :: JoinList (Score, Size) String -> Int
  value (Single (Point i, _) _) = i
  value Empty = 0
  value (Append (Point i, _) jl1 jl2) = i

main = runEditor editor (fromString . unlines $
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: JoinList (Score, Size) String)