-- Raise x to the power y, using recursion
-- For example, power 5 2 = 25
power :: Float -> Int -> Float -- modified to accept float base and return float (e.g. power 2 (-5) = 1/32)
power x y 
    | y == 0 = 1
    | y > 0 = x * power x (y-1)
    | otherwise = 1 / (power x (-1*y))

-- create a list of length n of the fibbonaci sequence in reverse order
-- examples: fib 0 = [0]
-- 	     fib 1 = [1, 0]
--	     fib 10 = [55,34,21,13,8,5,3,2,1,1,0]	
-- try to use a where clause
fib :: (Num a, Eq a) => a -> [a]
fib x 
    | x == 0 = [0]
    | x == 1 = [1, 0]
    | otherwise = (sum (take 2 remaining)) : remaining 
    where remaining = fib (x-1)

-- This is not recursive, but have a go anyway.
-- Create a function which takes two parameters, a number and a step
-- The result is the sign of the original number reversed, and the step added to the absolute value
-- Confused? Some examples: stepReverseSign 6 2 = -8
--			    stepReverseSign -3 1 = 4
--			    stepReverseSign 1 2 = -3
stepReverseSign :: (Fractional a, Ord a) => a -> a -> a
stepReverseSign x y
    | x >= 0 = -1 * val
    | x < 0 = val
    where val = abs x + abs y

{- Lets calculate pi.
 - The Leibniz formula for pi (http://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80)
 - Can be defined as pi = (4/1) - (4/3) + (4/5) - (4/7) ....
 - We can create a function, where given a certain tolerance, we can recursively calculate
 - Pi to within that tolerance.
 - Lets create two functions, piCalc, and piCalc', the latter we will recursively call
 - until our pi calculation is within the tolerance

 - The piCalc function is defined as:
 - piCalc :: (Fractional a, Integral b, Ord a) => a -> (a, b)

 - Given a tolerance, say, 0.001, it will return a tuple.
 - fst is pi to an accuracy of the tolerance, 0.001 in this case
 - snd is the number of recursive steps taken to calculate it, after all this chapter is about recursion!
 - Example: piCalc 0.001 = (3.1420924036835256,2000)

 - The piCalc' function is defined as 
 - piCalc' :: (Ord a, Fractional a, Integral b) => a -> a -> a -> b -> (a, b)
 - Lots of parameters!
 - The first parameter is the current denominator from the Leibniz formula
 - The next is our calculation of pi from our previous attempt
 - The next is the tolerance
 - The final parameter is the number of times this function has been called (ie, we add one every time we recurse
 - Example piCalc' 1 0.0 0.001 0 = (3.1420924036835256,2000)
 -
 - Feel free to change the parameter order, what parameters you need etc in order to get this to work for you,
 - But, of course the output of piCalc should remain as (pi, count)
 - 
 - You may find the stepReverseSign function handy
 -}

-- a = tolerance
piCalc :: Float -> (Float, Int)
piCalc a = piCalc' 0.0 0 a

-- x = calculation pi so far
-- y = number of times called
-- z = tolerance
piCalc' :: Float -> Int -> Float -> (Float, Int)
piCalc' x y z
    | z > delta = (x, y)
    | otherwise = piCalc' (x + (sign * term)) (y + 1) z
    where sign = (-1) ** (fromIntegral y)
          term = 4 / (fromIntegral(y) * 2 + 1)
          delta = abs (x - pi)
