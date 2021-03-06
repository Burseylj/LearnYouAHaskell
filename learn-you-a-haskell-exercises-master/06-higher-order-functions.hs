-- implementing functions from the chapter (Louis)

zipWith' :: (a -> b -> c) ->[a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort ( filter (<= x) xs )
        biggerSorted = quicksort( filter (> x) xs)
    in smallerSorted ++ [x] ++ biggerSorted
    
sumOddSq = sum (takeWhile (<10000) [n^2 | n <- [1,3..] ] )

collatzSequence :: (Integral a) => a -> [a]
collatzSequence 1 = [1]
collatzSequence n
   |even n   = n: collatzSequence (n `div` 2) 
   |odd n    = n: collatzSequence (n*3 + 1)
   
numLongCollatzSequence = length (filter isLong (map collatzSequence [1..100]))  
    where isLong xs = length xs > 15

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys 

clength :: Int -> Int
clength = length . collatzSequence 
    
    
    
-- Sum the numbers between two inclusive values recursively, assuming a < b when the function is first called
-- Example: sumInts 0 1 = 1
--          sumInts 1 3 = 6
sumInts :: Int -> Int -> Int
sumInts a b
    | a + 1     >= b = a + b
    |otherwise  = 1 + sumInts (a+1) b

-- Define a square function
sq :: Int -> Int
sq x = x*x

-- Sum the squares between two numbers. This function should be similar to the sumInts function
sumSquares :: Int -> Int -> Int
sumSquares a b
    | a + 1   >= b = sq a + sq b
    | otherwise   = sq a + sumSquares (a+1) b

-- Define a higher order sum function which accepts an (Int -> Int) function to apply to all integers between two values.
-- Again this should look similar to the sumInts and sumSquares functions
higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
higherOrderSum f a b = sum $ map f [a .. b]

-- Define the square sum in terms of higherOrderSum
hoSumSquares :: Int -> Int -> Int
hoSumSquares = higherOrderSum (^2)

-- Define the sum between two values in terms of higherOrderSum
-- Note there is no parameter on the function definition
-- Try to use a lambda if possible
hoSumInts :: Int -> Int -> Int
hoSumInts = higherOrderSum (\x-> x) 

-- Create a new higher order method which generalises over the function provided by sumInts (That is, parameterize (+) :: Int -> Int -> Int) between a and b
-- This will give the ability to perform utilities such as the prodcut of all squares (or any other Int -> Int function) between a and b
-- You will also need to generalise the base case
-- You can also define the function signature yourself, which leaves you free to define the parameters and their order
-- To be clear, your function will need to handle:
--  - A start value, a :: Int
--  - A end value, b :: Int
--  - A function to apply to each value, op :: Int -> Int
--  - A function to apply between each value, f :: Int -> Int -> Int
--  - A value to return in the base case when a > b, z :: Int
higherOrderSequenceApplication :: Int -> Int -> (Int -> Int) ->(Int->Int->Int) -> Int -> Int
higherOrderSequenceApplication a b op f z
    | a > b     = z
    | otherwise = (op a) `f` (higherOrderSequenceApplication (a+1) b op f z)

-- Define a factorial method using the higherOrderSequenceAppliction
hoFactorial :: Int -> Int
hoFactorial n = higherOrderSequenceApplication 1 n id (*) 1
