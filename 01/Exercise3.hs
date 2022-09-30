{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

module Exercise3 where
import Exercise1 (toDigits)


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (i : is) = sum (toDigits i) + sumDigits is