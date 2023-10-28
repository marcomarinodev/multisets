module MultiSet 

(
  MSet (..),
  empty,
  addElem,
  add,
  occs,
  elems,
  subeq,
  union,
  mapMSet,
)
where 

-- list of pairs where first element is the element itself
-- the second one is the multiplicity of that element in the multiset
-- An MSet is well-formed if for each of its pairs (v, n):
-- n > 0 && it does not contain (v, n) and (v', n') such that v = v'
data MSet a = MS [(a, Int)] deriving (Show)

-- it returns an empty MSet
empty :: MSet a
empty = MS []

-- it adds an element with occurence equals to m 
addElem :: (Eq a) => a -> Int -> MSet a -> MSet a
addElem v m (MS pairs) = MS (addHelper pairs)
  where
    addHelper [] = [(v, m)]
    addHelper (x:xs)
      | (fst x == v) = (fst x, snd x + m) : xs
      | otherwise = x : addHelper xs

-- it adds an element to the passed multiset
add :: (Eq a) => MSet a -> a -> MSet a
add mset v = addElem v 1 mset

-- it returns the number of occurrences of an element in the passed
-- multiset
occs :: (Eq a) => MSet a -> a -> Int
occs (MS pairs) v = occsHelper pairs
  where
    occsHelper [] = 0
    occsHelper (x:xs)
      | (fst x == v) = snd x
      | otherwise = occsHelper xs

-- it returns a list containing all the elements of the passed multiset
elems :: MSet a -> [a]
elems (MS []) = []
elems (MS (x:xs)) = replicate (snd x) (fst x) ++ elems (MS xs)

-- it returns true if the first MSet is subset of the second one
subeq :: (Eq a) => MSet a -> MSet a -> Bool
subeq (MS pairs1) mset2 = all (\(v, k) -> k <= occs mset2 v) pairs1

-- it returns a set composed by the union of mset1 and mset1
union :: (Eq a) => MSet a -> MSet a -> MSet a
union mset1 (MS []) = mset1
union mset1 (MS (x:xs)) = union (addElem (fst x) (snd x) mset1) (MS xs)

-- defining MSet to be instance of Eq to check if two MSets are equal
-- two MSets are equal if both MSets are subsets of each other
instance (Eq a) => Eq (MSet a) where
  (==) :: (Eq a) => MSet a -> MSet a -> Bool
  mset1 == mset2 = subeq mset1 mset2 && subeq mset2 mset1

-- defining MSet to be instance of Foldable. The minimum set of
-- functions to implement consist of implementing the foldr function
-- (\x -> f (fst x)) applies the f function to a certain element of
-- the set, not the multiplicity of that element
instance Foldable MSet where
  foldr :: (a -> b -> b) -> b -> MSet a -> b
  foldr f acc (MS pairs) = foldr (\x -> f (fst x)) acc pairs
  
mapMSet :: (Eq b) => (a -> b) -> MSet a -> MSet b 
mapMSet f (MS pairs) = mapMSetHelper pairs
  where
    mapMSetHelper [] = MS []
    mapMSetHelper ((x,mul):xs) = addElem (f x) mul (mapMSetHelper xs)

-- main :: IO()
-- main = do
--   print (occs (add (MS [('a', 1), ('b', 2), ('c', 3)]) 'c') 'c')
--   print (elems (MS [('a', 1), ('b', 2), ('c', 3)]) )
--   print (subeq (MS [('b', 8)]) (MS [('a', 2),('b', 8),('c', 1)]))
--   print (union (MS [('a', 1), ('b', 2), ('c', 3)]) (MS [('b', 8)]))
--   print (foldr (++) "" (MS [("hi", 1), ("how", 2), ("what", 3)]))
--   testMapMSet
