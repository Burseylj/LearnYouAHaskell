-- my attempts at programs from the text (Louis)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate (n-1) x 

take' :: (Num n, Ord n) => n -> [a] -> [a]
take' n _
    | n <= 0  = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x] 

repeat' :: x -> [x]
repeat' x = x: repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip'  [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a-> [a] -> Bool
elem' _ [] = False
elem' i (x:xs)
    | i == x  = True
    | otherwise = elem' i xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a<- xs, a<= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted


-- Raise x to the power y, using recursion
-- For example, power 5 2 = 25
power :: Int -> Int -> Int
power x y 
    | x == 0  || y <= 0 = undefined
    | y == 0 = 1
    | y > 0 = x * power x (y - 1)

-- create a list of length n of the fibbonaci sequence in reverse order
-- examples: fib 0 = [0]
-- 	     fib 1 = [1, 0]
--	     fib 10 = [55,34,21,13,8,5,3,2,1,1,0]	
-- try to use a where clause
fib :: (Num a, Eq a) => a -> [a]
fib x 
    | x == 0 = [0]
    | x == 1 = [1,0]
    | otherwise = (y + z) : all 
    where all@(y:z:_) = fib (x-1)

-- This is not recursive, but have a go anyway.
-- Create a function which takes two parameters, a number and a step
-- The result is the sign of the original number reversed, and the step added to the absolute value
-- Confused? Some examples: stepReverseSign 6 2 = -8
--			    stepReverseSign -3 1 = 4
--			    stepReverseSign 1 2 = -3
stepReverseSign :: (Fractional a, Ord a) => a -> a -> a
stepReverseSign a
    | a < 0 = (+) (abs a)
    | otherwise = (-) (-a)

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

piCalc :: (Fractional a, Integral b, Ord a) => a -> (a, b)
piCalc a = piCalc' 1 0 a 0

piCalc' :: (Ord a, Fractional a, Integral b) => a -> a -> a -> b -> (a, b)
piCalc' denom val tol calls
    | toAdd < tol = (val, calls)
    |otherwise  =  piCalc' (denom + 2) (stepReverseSign (4 / denom) val) tol (calls + 1)
    where toAdd = 4/denom
          

