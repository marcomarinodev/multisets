module Main where

-- recursive solution
myReplicate :: (Num a, Ord a) => a -> b -> [b]
myReplicate n _ | n <= 0 = []
myReplicate n v = v : myReplicate (n - 1) v

-- solution with combinators
myReplicate2 :: Int -> b -> [b]
myReplicate2 n v = take n (repeat v)

main = do
  print (myReplicate 3 "a")
  print (myReplicate2 3 "b")