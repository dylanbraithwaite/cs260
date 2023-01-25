{-For these exercises it is recommended that you use Stack to run GHCI. 
Once you have downloaded and saved this file you should open a terminal and navigate to the correct folder.
 
 Once you are in the correct folder run ghci using Stack, 
 you will then be able to load this file using :l Ex1.hs . 
 
 Define the required functions in the file using the already provided names and types! 
 Do not change the naming or typing of functions. 
 When defining a function f replace the line f = undefined with your definition. -} 

-- 1) Define the following:
--(a) a checkSum function that takes input a, b and c evaluating to true when a + b = c
  
checkSum:: Int -> Int -> Int -> Bool
checkSum a b c = a + b == c

--(b) Define a function notdivisible :: Int -> Int -> Bool that evaluates to True on input a and b 
--     if and only if a is not divisible by b.

divisible :: Int -> Int -> Bool
divisible a b = a `mod` b == 0

notDivisible :: Int -> Int -> Bool
notDivisible a b = not (divisible a b)

--2) Define evenFactorial which, for a value n, multiplies all of the even numbers less than or equal to n.
--    For example evenFactorial 6 should return 6*4*2.

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * (factorial (n-1))

evenFactorial:: Int -> Int
-- evenFactorial 0 = 1
evenFactorial n | n <= 0         = 1
                | n `mod` 2 == 1 = evenFactorial (n-1)
                | otherwise      = n * evenFactorial (n -2 )


{-3)
  a)The Collatz function is defined as follows:
f(x) = { x/2,    if x is even
       { 3x + 1, if x is odd
Define this function in Haskell (you may want to use div instead of /). -}

--- 5 -> 3 * 5 + 1 = 16 -> 8 -> 4 -> 2 -> 1 -> 4 

-- (/) :: Int -> Int -> Float
-- div :: Int -> Int -> Int

collatz :: Int -> Int 
collatz n | n `mod` 2 == 0 = n `div` 2 -- even
          | otherwise      = 3 * n + 1 -- odd

--b) The collatz conjecture states that for any integer n repeated application of the collatz function 
--    will eventually reduce n to 1. 
--    Write a function which will count the number of applications needed to return a value of 1. 
--    The first argument should count the number of applications while the second holds the current 
--    value of n

-- 5
-- collatz 5 = 16
-- collatz (collatz 5) = 8

-- collatzApp 0 5
-- collatzApp (1) (collatz 5) == collatzApp 1 16
-- collatzApp 2 8
-- collatzApp 3 4
-- collatzApp 4 2
-- collatzApp 5 1

collatzApp :: Int -> Int -> Int
collatzApp c 1 = c
collatzApp c n = collatzApp (c+1) (collatz n)



--Note. The collatz conjecture has yet to be proven, despite its simple appearance. 
--  Here is some code that will return the number of needed applications for every number from 1 to 1000. 

testCollatz = map (collatzApp 0) [1..1000]

--4) Define fibonnaci, which calculates the nth number of the fibonacci sequence. 
--    Challenge: try to avoid using the standard recursive definition. 

-- 1, 1, 2, 3, 5, 8, 13, ...

-- fibonacciHelper 6 1 1
-- fibonacciHelper 5 1 2
-- fibonacciHelper 4 2 3

fibonacciHelper :: Int -> Int -> Int -> Int
fibonacciHelper 0 a b = a
fibonacciHelper n a b = fibonacciHelper (n-1) b (a + b)

-- fibonacciHelper m a b <-- a == fib n and b == fib (n + 1) 

fibonacci:: Int -> Int 
-- fibonacci 1 = 1
-- fibonacci 2 = 1
-- fibonacci n = fibonacci (n-1) + fibonacci (n-2)
fibonacci n = fibonacciHelper (n-1) 1 1

--5) Defined the combinatorial function for numbers n and m given by n!/(m!*(n-m)!), 
--    this should return an error if n < m

comb :: Int -> Int -> Int 
comb n m | n < m     = error "n should be >= m"
         | otherwise = (factorial n) `div` ((factorial m) * factorial (n - m))

--6) Define leapYear which returns a String telling the user whhether a supplied year is a leap year.

isLeapYear :: Int -> Bool
isLeapYear n = divisible n 4

leapYear::Int -> String
leapYear n | isLeapYear n = "That is a leap year"
           | otherwise    = "That is not a leap year"

--7) Define the function prime which returns True if a given number n is prime. 
--   Remember that a number is prime if it is not divisible by any number other than 1 and itself. 
--   It may help to define a helper function

-- 8
-- 7 divide 8? no
-- 6 divide 8? no
-- 5? no
-- 4? yes

primeHelper :: Int -> Int -> Bool
primeHelper n 1 = True
primeHelper n m | m >= n        = error "First argument should be larger than second"
                | divisible n m = False
                | otherwise     = primeHelper n (m-1) 

prime :: Int -> Bool 
prime 1 = False
prime n = primeHelper n (n-1)
