{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
import Control.Arrow ((>>>))
import Data.Bifunctor (first)
import Data.List ((\\))


--Exercise 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr f 1
    where
    f x s = if even x then (x-2)*s else s


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n  | even n = n + fun2 (n `div` 2)
        | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = iterate f >>> takeWhile (/= 1) >>> filter even >>> sum
    where
        f n = if even n then n `div` 2 else 3*n+1


-- Exercise 2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

minHeight :: Tree a -> Integer
minHeight Leaf = 0
minHeight (Node i t c Leaf) = i+1
minHeight (Node i Leaf c t) = i+1
minHeight (Node i t1 c t2) = min (minHeight t1) (minHeight t2)

foldTree :: [a] -> Tree a
foldTree = foldr f Leaf
    where
    f :: a -> Tree a -> Tree a
    f c t = fst $ p c n t n
        where
        n = minHeight t
        p :: a  -> Integer -> Tree a -> Integer -> (Tree a, Bool)
        p c nn Leaf 0 = (Node nn Leaf c Leaf, True)
        p c nn Leaf i = (Leaf, False)
        p c nn t@(Node i t1 c' t2) j = if snd r1 then first (\t -> Node i t c' t2) r1 else first (Node i t1 c') r2
            where
            r1 = p c nn t1 (j-1)
            r2 = p c nn t2 (j-1)

main = print (foldTree "ABCDEFGHIJ")

-- Exercise 3

xor :: [Bool] -> Bool
xor = foldr x_or False
    where
    x_or :: Bool -> Bool -> Bool
    x_or True b = not b
    x_or False b = b

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\ x l -> f x : l) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

-- Exercise 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\i -> 2*i + 1) ([1..n] \\ [i+j+2*i*j | i <- [1..n], j <- [1..i]])


