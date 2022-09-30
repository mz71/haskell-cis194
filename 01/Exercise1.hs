
module Exercise1 where
    toDigits    :: Integer -> [Integer]
    toDigits 0 = []
    toDigits i
        | i < 0     = []
        | otherwise =  helper i [] where
            helper :: Integer -> [Integer] -> [Integer]
            helper 0 is = is
            helper j is = helper (div j 10) (mod j 10 : is)

    toDigitsRev :: Integer -> [Integer]
    toDigitsRev 0 = [0]
    toDigitsRev i 
        | i < 0     = []
        | otherwise = mod i 10 : toDigitsRev (div i 10)
