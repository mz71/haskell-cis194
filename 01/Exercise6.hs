import Exercise5

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 i p1 p2 p3 p4
    | i <= 0 = []
    | i == 1 = [(p1, p2)]
    | i == 2 = [(p1, p3), (p1, p2), (p3, p2)]
    | otherwise = hanoi4 (i-2) p1 p3 p2 p4 ++ ([(p1, p4),(p1, p2), (p4, p2)] ++ hanoi4 (i-2) p3 p2 p1 p4)

hanoi4' i p1 p2 p3 p4
    | i <= 3 = hanoi4 i p1 p2 p3 p4
    | otherwise = hanoi4' (i-3) p1 p3 p2 p4 ++ hanoi 3 p1 p2 p4 ++ hanoi4' (i-3) p3 p2 p1 p4

hanoi4'' i p1 p2 p3 p4
    | i <= 3 = hanoi4 i p1 p2 p3 p4
    | otherwise = hanoi4' (i-4) p1 p3 p2 p4 ++ hanoi 4 p1 p2 p4 ++ hanoi4' (i-4) p3 p2 p1 p4

hanoi4''' j i p1 p2 p3 p4
    | i <= j = hanoi4 i p1 p2 p3 p4
    | otherwise = hanoi4''' j (i-j) p1 p3 p2 p4 ++ hanoi j p1 p2 p4 ++ hanoi4''' j (i-j) p3 p2 p1 p4

lb i
    | i <= 1 = 1
    | otherwise = lb(i `div` 2) + 1

tria i
    | i <= 2 = 1
    | i <= 5 = 2
    | otherwise = tria' 1 i
        where
            tria' n i
                | i < 0 = n-2
                | otherwise = tria' (n+1) (i-n)


hanoi4_knuth_general f i p1 p2 p3 p4
    | i <= 3 = hanoi4 i p1 p2 p3 p4
    | otherwise = hanoi4_knuth_general f (i-j) p1 p3 p2 p4 ++ hanoi j p1 p2 p4 ++ hanoi4_knuth_general f (i-j) p3 p2 p1 p4
        where
            j = f i

hanoi4_knuth = hanoi4_knuth_general tria

hanoi4_mati = hanoi4_knuth_general lb