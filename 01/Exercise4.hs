import Exercise1
import Exercise2
import Exercise3
import Control.Arrow

validate :: Integer -> Bool
validate = toDigits >>> doubleEveryOther >>> sumDigits >>> (\ i -> mod i 10 == 0)