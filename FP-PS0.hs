-- Q1 maxima
myMax :: Int -> Int -> Int
myMax x y 
    | x > y     = x
    | otherwise = y

myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:xs)
    | x > maxXs = x
    | otherwise = maxXs
    where maxXs = myMaximum xs


-- Q2 nth Fibonacci number
fib :: Int -> Int
-- just sets up the base cases, then formats nicely from fibInner
fib 1 = 1
fib 2 = 1
fib n = head (fibInner [1, 1, n])

fibInner:: [Int] -> [Int]
-- stores 2 nums at a time, and n represents the nth number
-- stops at 3, since n=1 & 2 are taken care of by fib
fibInner [current, prev, 3] = [current + prev]
fibInner [current, prev, n] = fibInner [current + prev, current, n-1]
-- for n ≥ 93, Int is no longer big enough


-- Q3 shortest definition of factorial I can think of
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)


-- Q4 Bools
-- expressions of type Bool: True, False, True && False, (True && False) || True

-- function of type Bool -> (Bool -> Bool)
boolFunc1 :: Bool -> (Bool -> Bool)
-- if initial Bool is True, the function keeps the next value the same
-- if initial Bool is False, the function flips the next value
boolFunc1 a b = a == b

-- function of type (Bool -> Bool) -> Bool
boolFunc2 :: (Bool -> Bool) -> Bool
boolFunc2 f = f True

-- How many different functions of type Bool -> Bool are there?
-- There are 2 possible inputs, True and False. For each input, there are 2 possible outputs, True and False.
-- So, there are 2x2 ways to pick the outputs.
-- Hence, there are 4 different functions of type Bool -> Bool


-- Q5 gives list of first elements
inits :: [[a]] -> [a]
inits [] = []
inits [x] = [head x]
inits (x:xs) = head x : inits xs


-- Q6 partitions
partitions :: [a] -> [([a], [a])]
partitions a = [(take n a, drop n a) | n <- [0..(length a)]]