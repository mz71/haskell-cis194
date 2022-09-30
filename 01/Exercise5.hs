
module Exercise5 where
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi i p1 p2 p3
    | i <= 0 = []
    | i == 1 = [(p1,p2)]
    | otherwise = hanoi (i-1) p1 p3 p2 ++ ((p1,p2) : hanoi (i-1) p3 p2 p1)