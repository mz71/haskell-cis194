module Exercise2 where
count :: [Integer] -> Integer
count [] = 0
count (i : is) = count is + 1

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther is
    | even (count is) = doubleEveryOther_0 is
    | otherwise = doubleEveryOther_1 is
    where
        doubleEveryOther_0 (x : y : js) = x+x : y : doubleEveryOther_0 js
        doubleEveryOther_0 [x] = [x+x]
        doubleEveryOther_0 [] = []
        doubleEveryOther_1 (x : y : js) = x : y+y : doubleEveryOther_1 js
        doubleEveryOther_1 [x] = [x]
        doubleEveryOther_1 [] = []