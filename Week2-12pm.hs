{-For these exercises it is recommended that you use Stack to run GHCI. Once you have downloaded and saved this file you should open a terminal and navigate to the correct folder. Once you are in the correct folder run ghci using Stack, you will then be able to load this file using :l Ex1.hs . Define the required functions in the file using the already provided names and types! Do not change the naming or typing of functions. When defining a function f replace the line f = undefined with your definition. -} 

-- 1) Define the following:
--(a) a checkSum function that takes input a, b and c evaluating to true when a + b = c
  
checkSum:: Int -> Int -> Int -> Bool
checkSum = undefined

--(b) Define a function notdivisible::Int -> Int -> Bool that evaluates to True on input a and b if and only if a is not divisible by b.
 
notDivisible::Int -> Int -> Bool
notDivisible = undefined

--2) Define evenFactorial which, for a value n, multiplies all of the even numbers less than or equal to n. For example evenFactorial 6 should return 6*4*2.

evenFactorial:: Int -> Int
evenFactorial n = undefined


{-3)
  a)The Collatz function is defined as follows:
f(x) = { x/2,    if x is even
       { 3x + 1, if x is odd
Define this function in Haskell (you may want to use div instead of /). -}

collatz:: Int -> Int 
collatz n = undefined

--b) The collatz conjecture states that for any integer n repeated application of the collatz function will eventually reduce n to 1. Write a function which will count the number of applications needed to return a value of 1. The first argument should count the number of applications while the second holds the current value of n

collatzApp:: Int -> Int -> Int
collatzApp c n = undefined

--Note. The collatz conjecture has yet to be proven, despite its simple appearance. Here is some code that will return the number of needed applications for every number from 1 to 1000. 

testCollatz = map (collatzApp 0) [1..1000]

--4) Define fibonnaci, which calculates the nth number of the fibonacci sequence. Challenge: try to avoid using the standard recursive definition. 

fibonacci:: Int -> Int 
fibonacci = undefined 

--5) Defined the combinatorial function for numbers n and m given by n!/(m!*(n-m)!), this should return an error if n < m

comb::Int -> Int -> Int 
comb = undefined 

--6) Define leapYear which returns a String telling the user whhether a supplied year is a leap yer.

leapYear::Int -> String
leapYear = undefined

--7) Define the function prime which returns True if a given number n is prime. Remember that a number is prime if it is not divisible by any number other than 1 and itself. It may help to define a helper function

prime::Int -> Bool 
prime = undefined 