{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)


-- Exercise 1
fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = map fst (iterate (\ (a,b) -> (b, a+b)) (0,1))

-- Exercise 3

data Stream a =
    Sons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Sons x sx) = x : streamToList sx

instance Show a => Show (Stream a) where
    show a = "( " ++ showHelper a 20 ++ "..)"
        where
        showHelper :: Show a => Stream a -> Int -> String
        showHelper (Sons x sx) 0 = show x
        showHelper (Sons x sx) i = show x ++ (',' : showHelper sx (i-1))

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Sons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Sons x sx) = Sons (f x) (streamMap f sx)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Sons x (streamFromSeed f (f x))

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

posNats = streamFromSeed (+1) 1

ruler :: Stream Integer
ruler = streamMap lb posNats
    where
    lb :: Integer -> Integer
    lb x | x == 0 = 2^666
         | even x = lb (x `div` 2) + 1
         | otherwise = 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Sons x sx) s = Sons x (interleaveStreams s sx)

interleaveStreams_Mega :: Stream (Stream a) -> Stream a
interleaveStreams_Mega (Sons sa ss) = interleaveStreams sa (interleaveStreams_Mega ss)

ruler_Clever :: Stream Integer
ruler_Clever = interleaveStreams_Mega (streamMap streamRepeat nats)

main :: IO ()
main = print ruler_Clever

-- Exercise 6

x :: Stream Integer
x = Sons 0 (Sons 1 (streamRepeat 0))


(*/) :: Num a => a -> Stream a -> Stream a
a */ (Sons x sx) = Sons (a * x) (a */ sx)

instance Num a => Num (Stream a) where
  (+) :: Stream a -> Stream a -> Stream a
  (Sons x sx) + (Sons y sy) = Sons (x+y) (sx + sy)
  (*) :: Stream a -> Stream a -> Stream a
  (Sons x sx) * b@(Sons y sy) = Sons (x * y) (x */ sy + sx*b)

  fromInteger :: Num a => Integer -> Stream a
  fromInteger i = Sons (fromInteger i) (streamRepeat 0)
  negate :: Stream a -> Stream a
  negate (Sons x sx) = Sons (negate x) (negate sx)

instance Fractional (Stream Integer) where
    (/) ::  Stream Integer -> Stream Integer -> Stream Integer
    (Sons x sa) / (Sons b sb) = q where
        q = Sons (x `div` b) ((1 `div` b) */ (sa - q * sb))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7

data Matrix2 = Mat2 Integer Integer Integer Integer
    deriving Show

instance Num Matrix2 where
  (+) :: Matrix2 -> Matrix2 -> Matrix2
  Mat2 a b c d + Mat2 a' b' c' d' = Mat2 (a+a') (b+b') (c+c') (d+d')
  (*) :: Matrix2 -> Matrix2 -> Matrix2
  Mat2 a b c d * Mat2 a' b' c' d' = Mat2 (a*a' + b*c') (a*b' + b*d') (c * a' + d * c') (c * b' + d * d')
  fromInteger :: Integer -> Matrix2
  fromInteger i = Mat2 i 0 0 i

lu :: Matrix2 -> Integer
lu (Mat2 a b c d) = a

ld :: Matrix2 -> Integer
ld (Mat2 a b c d) = c

fib4 :: Integer -> Integer
fib4 n = ld (Mat2 1 1 1 0 ^ n)