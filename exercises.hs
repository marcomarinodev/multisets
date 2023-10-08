module Main where

import Data.List (isPrefixOf)
import Data.Char (toUpper)

-- you can find the exercises here
-- https://pages.di.unipi.it/corradini/Didattica/AP-22/ESER/03/exercises_3.html

-- EXERCISE 1
-- recursive solution
myReplicateR :: (Num a, Ord a) => a -> b -> [b]
myReplicateR n _ | n <= 0 = []
myReplicateR n v = v : myReplicateR (n - 1) v

test1 :: IO()
test1 = do 
  print ("--- Exercise 1 ---")
  print (myReplicateR 3 "a" == myReplicateC 3 "a")
  print (myReplicateR 0 (-1) == myReplicateC 0 (-1))

-- solution with combinators
myReplicateC :: Int -> b -> [b]
myReplicateC n v = take n (repeat v)

-- EXERCISE 2
-- recursive solution
sumOddR :: [Int] -> Int
sumOddR [] = 0
sumOddR (x:xs) = if x `mod` 2 /= 0 
  then x + sumOddR xs else sumOddR xs

-- solution with combinators
isOdd :: Int -> Bool
isOdd x = x `mod` 2 /= 0

sumOddC :: [Int] -> Int
sumOddC x = sum (filter (isOdd) x )

test2 :: IO()
test2 = do
  print ("--- Exercise 2 ---")
  print (sumOddR [1,2,3,4,5,6] == sumOddC [1,2,3,4,5,6])
  print (sumOddR [] == sumOddC [])

-- EXERCISE 3
replR :: [Int] -> Int -> [Int]
replR _ n | n <= 0 = []
replR xs n = xs ++ replR xs (n - 1) 

replC :: [Int] -> Int -> [Int]
replC xs n = take (length xs * n) (cycle xs)

test3 :: IO()
test3 = do
  print ("--- Exercise 3 ---")
  print (replR [1,2,3] 3 == replC [1,2,3] 3)
  print (replR [1,2,3] (-1) == replC [1,2,3] (-1))

-- EXERCISE 4
totalLengthR :: [String] -> Int
totalLengthR [] = 0
totalLengthR (x:xs) = if (head  x == 'A')   
  then length x + totalLengthR xs
  else totalLengthR xs

startsWithA :: String -> Bool
startsWithA xs = head xs == 'A'

totalLengthC :: [String] -> Int
totalLengthC xs = sum (map (length) (filter (startsWithA) xs)) 

test4 :: IO()
test4 = do 
  print ("--- Exercise 4 ---")
  print (totalLengthR ["Albuquerque", "Table", "Air", "Affiliated", "God"]
    == totalLengthC ["Albuquerque", "Table", "Air", "Affiliated", "God"])

-- EXERCISE 5
filterOddHelperR :: [a] -> Int -> [a]
filterOddHelperR [] _ = []
filterOddHelperR (x:xs) idx = if idx `mod` 2 == 0 
  then filterOddHelperR xs (idx + 1)
  else x : filterOddHelperR xs (idx + 1)

filterOddR :: [a] -> [a]
filterOddR xs = filterOddHelperR xs 0 

filterOddC :: [a] -> [a]
filterOddC xs = map (snd) (filter (isOdd . fst) (zip [0..(length xs)] xs))

test5 :: IO()
test5 = do 
  print ("--- Exercise 5 ---")
  print (filterOddR [1,2,3,4,5,6] == filterOddC [1,2,3,4,5,6])

-- EXERCISE 6
titleCaseRHelper :: [String] -> [String]
titleCaseRHelper [] = []
titleCaseRHelper (x:xs) = ((toUpper (head x)) : (tail x)) : titleCaseRHelper xs

titleCaseR :: String -> String 
titleCaseR s = unwords (titleCaseRHelper (words s))

titleCaseC :: String -> String
titleCaseC s = unwords (map (\x -> toUpper (head x) : (tail x)) (words s)) 

test6 :: IO()
test6 = do
  print ("--- Exercise 6 ---")
  print (titleCaseR "ciao a tutti" == titleCaseC "ciao a tutti")

-- EXERCISE 7
countVowels :: String -> Int -> Int 
countVowels "" _ = 0
countVowels (x:xs) cnt = if (x `elem` "aeiouAEIOU")
  then 1 + countVowels xs cnt
  else countVowels xs cnt

isPalindrome :: String -> Bool
isPalindrome "" = True
isPalindrome xs | length xs == 1 = True
isPalindrome (x:xs) = if (x == last xs) 
  then isPalindrome (tail (reverse xs))
  else False

countVowelPaliR :: [String] -> Int
countVowelPaliR [] = 0
countVowelPaliR (x:xs) = if (isPalindrome x)
  then countVowels x 0 + countVowelPaliR xs
  else countVowelPaliR xs

countVowelPaliC :: [String] -> Int
countVowelPaliC xs = sum [ length (filter (`elem` "aeiouAEIOU") x)  | x <- xs, x == reverse x ]

test7 :: IO()
test7 = do
  print ("--- Exercise 7 ---")
  print (countVowelPaliR ["anna", "banana", "civic", "mouse"] ==
     countVowelPaliC ["anna", "banana", "civic"])

-- EXERCISE 8
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldl (\acc x -> acc ++ [f x]) [] xs

test8 :: IO()
test8 = do
  print ("--- Exercise 8 ---")
  print (myMap (+2) [1,2,3] == map (+2) [1,2,3])

-- EXERCISE 9
data IntTree = Leaf Int | Node (Int, IntTree, IntTree)

describeTree :: IntTree -> String
describeTree (Leaf x) = "(Leaf: " ++ show x ++ ")"
describeTree (Node (x, l, r)) = " [Node = (Leaf: " ++ show x ++ ", l: "
  ++ describeTree l ++ ", r: " ++ describeTree r ++ ")] "

tmap :: (Int -> Int) -> IntTree -> IntTree
tmap f (Leaf x) = Leaf (f x) 
tmap f (Node (x, left, right)) = Node((f x), tmap f left, tmap f right)

sampleTree :: IntTree
sampleTree = Node (1, Node (2, Leaf 3, Leaf 4), Leaf 5)

succTree :: IntTree -> IntTree
succTree t = tmap (+1) t 

sumSuccHelper :: IntTree -> Int -> Int
sumSuccHelper (Leaf x) acc = acc + x + 1
sumSuccHelper (Node (x, l, r)) acc = acc + x + 1 
  + sumSuccHelper l 0 + sumSuccHelper r 0  

sumSucc :: IntTree -> Int
sumSucc (Leaf x) = x + 1
sumSucc (Node (x, l, r)) = sumSuccHelper (Node (x, l, r)) 0

test9 :: IO()
test9 = do
  print ("--- Exercise 9 ---")
  print (describeTree sampleTree)
  print (" *** ")
  print (describeTree (succTree sampleTree))
  print (sumSucc sampleTree)

performTests :: IO()
performTests = do
  test1
  test2
  test3
  test4
  test5
  test6
  test7
  test8
  test9

main = do
  performTests
