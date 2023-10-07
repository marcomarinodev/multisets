module Main where

concatenate :: [Int] -> [Int] -> [Int]
concatenate xs ys = xs ++ ys

data Person = Person
  { firstName :: String,
    lastName :: String
  }

-- functions
k xs =
  let cntTail [] = 0
      cntTail (y : ys) = length ys
   in cntTail xs

-- recursive functions
myreverse xs =
  let rev ([], acc) = acc
      rev (y : ys, acc) = rev (ys, y : acc)
   in rev (xs, [])

g :: (Int, Int) -> Int
g (a, b) = a + b * 2

-- currying and uncurrying
f :: Int -> (Int -> Int)
f = curry g

g_again :: (Int, Int) -> Int
g_again = uncurry f

removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

len xs = sum [1 | _ <- xs]

data Tree = Leaf Int | Node (Int, Tree, Tree)

-- case expression (1)
describeList :: [a] -> String
describeList xs =
  "The list is " ++ case xs of
    [] -> "empty."
    [x] -> "a singleton list."
    xs -> "a longer list."

-- case expression (2)
data Exp = Var Int | Const Int | Plus (Exp, Exp)

describeExpression :: Exp -> String
describeExpression e = case e of
  Var n -> show n
  Const n -> show n
  Plus (e1, e2) -> describeExpression e1 ++ " + " ++ describeExpression e2

-- in functional programming, for and while loops are replaced by using recursionù
-- take' n lst; it returns first n elements of a list
-- (Num i, Ord i) means that i must be a number and ordered. (Ord is required to compare numbers)
takeN :: (Num i, Ord i) => i -> [a] -> [a]
takeN n _ | n <= 0 = []
takeN _ [] = []
takeN n (x : xs) = x : takeN (n - 1) xs

-- higher-order functions (functions that take other functions as arguments, or return a function as result)
applyTo5 :: Num t1 => (t1 -> t2) -> t2
applyTo5 f = f 5

-- stream-like high-order functions
revTailSum :: Num a => [a] -> a
revTailSum xs = sum (tail (reverse xs))

printGeneralFeatures :: IO ()
printGeneralFeatures = do
  print (tail (concatenate [1, 2] [3, 4]))
  print (take 10 (cycle [1, 2]))
  print (k [1, 2, 3, 4, 5, 6])
  print (myreverse [1, 2, 3])
  print (g (2, 3))
  print (f 2 3)
  print (g_again (2, 3))
  print (len (removeNonUppercase "BruvAreYouMadCuz")) -- strings are lists
  print (describeList "t")
  print (describeExpression (Plus (Var 1, Plus (Const 3, Var 4))))
  print (takeN 3 [1, 2, 3, 4, 5, 6])
  print (applyTo5 (2 *)) -- apply multiplication by 2 to 5
  print (revTailSum [1, 2, 3, 4, 5])

-- reduce combinator (foldl, foldr, foldl1, foldr1)
-- foldl: fols values from beginning to end
foldSumL :: (Num a) => [a] -> a
foldSumL xs = foldl (\acc x -> acc + x) 0 xs

foldSumR :: (Num a) => [a] -> a
foldSumR xs = foldr (\acc x -> acc + x) 0 xs

-- searching a substring: exploiting laziness
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] x = True
isPrefixOf (y:ys) [] = False
isPrefixOf (y:ys) (x:xs) = if (x == y) then isPrefixOf ys xs else False

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes (x:xs) = (x:xs) : suffixes xs

isSubStringOf :: Eq a => [a] -> [a] -> Bool
x `isSubStringOf` s = or [x `isPrefixOf` t | t <- suffixes s]

main = do
  -- map combinator
  print (map (+ 3) [1, 2, 3, 4, 5])
  print (map (++ "!") ["one", "two", "threee"])
  print (map (replicate 2) [3 .. 5]) -- [[3,3], [4,4], [5,5]]
  print (map fst [(1, 2), (3, 5), (6, 3)]) -- [1,3,6]

  -- filter combinator
  let notNull x = not (null x)

  print (filter notNull [[1, 2, 3], [], [4, 5], [], []])

  -- reduce combinator
  let inputFoldArray = [1, 9, 3, 7, 5, 4]
  print (foldSumL inputFoldArray == foldSumR inputFoldArray)

  -- searching a substring
  print ("nce" `isSubStringOf` "francesco")
