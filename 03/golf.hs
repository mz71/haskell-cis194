{-# LANGUAGE ScopedTypeVariables #-}

module Golf where
import Data.List
import Data.Maybe (listToMaybe, fromJust)

--Exercise 1
skips :: [Integer] -> [[Integer]]
skips p = map (\ i -> unfoldr (uncons . drop i) p) [0 .. length p - 1]
{- 
We have to explain two things here:
    1. The solution is of the form : skips p = map f [0 .. length p - 1]
    where f :: Integer -> [a] such that
    f i is the list containing every ith element from the input list.
    If we use f for each number from 0 to length p - 1 we obtain the desired subarrays
    2. f i = unfoldr (uncons . drop i) p ... what does it mean???
    Well, uncons is a perfect match for unfoldr. They have matching input-output types! That's why it looks so simple!
    unfoldr generates a new list. It takes two arguments - a generator and a seed.
    The generator here is a function [Int] -> Maybe (Int,[Int]). It iterates the seed over and over to get the answer
-} 

localMaxima :: [Integer] -> [Integer]
localMaxima a = map fst $ filter (\ (x,(y,z)) -> y < x && x > z) $ zip (tail a) $ zip a $ tail $ tail a 

histogram :: [Integer] -> String
histogram as = foldr ((++) . (\i -> map ((\j -> if j > 0 then '*' else ' ') . (+(-i))) is ++ "\n" )) "==========\n0123456789\n" [m-1, m-2 .. 0] where
    is = map (\n ->  length (filter (== n) as)) [0..9]
    m = maximum is
