-- This function should print a single digit number as English text, or "unknown" if it's out of the range 0-9
englishDigit :: Int -> String
englishDigit x
    | elem x [0..9] = show x
    | otherwise = "unknown"

-- given a tuple, divide fst by snd, using pattern matching. 
-- it should return undefined for division by zero
divTuple :: (Eq a, Fractional a) => (a, a) -> a
divTuple (x, y)
    | y /= 0 = x / y
    | otherwise = undefined

-- if the first three numbers in a list are all zero, return True
threeZeroList :: [Int] -> Bool
threeZeroList (x1:x2:x3:xs) = x1 == 0 && x2 == 0 && x3 == 0
threeZeroList xs = False
