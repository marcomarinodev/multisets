module Main where

import Data.Char (toLower)
import Data.List (sort)
import MultiSet (MSet(..), add, empty, union, subeq)

-- ciao means "characters in an alphabetical order"
getCiao :: String -> String
getCiao [] = []
getCiao (x:xs) = sort ((toLower x) : getCiao xs)

-- it returns an MSet containing all the ciao of the words in a file

readMSet :: FilePath -> IO (MSet String)
readMSet filePath = do
  content <- readFile filePath
  return (buildMSet (map getCiao (words content)))
    where
      buildMSet :: [String] -> MSet String
      buildMSet [] = empty
      buildMSet (x:xs) = add (buildMSet xs) x

writeMSet :: FilePath -> MSet String -> IO()
writeMSet filePath (MS pairs) =
  writeFile filePath (unlines (map (\(v, k) -> "<" ++ v ++ ", " ++ show k ++ ">") pairs))

main :: IO()
main = do
  m1 <- readMSet "aux_files/anagram.txt"
  m2 <- readMSet "aux_files/anagram-s1.txt"
  m3 <- readMSet "aux_files/anagram-s2.txt"
  m4 <- readMSet "aux_files/margana2.txt"

  print("TEST: m1 != m4, but they have the same elements")
  if (subeq m1 m4 || subeq m4 m1) && m1 /= m4 
    then print("=> PASSED")
    else print("=> FAILED")

  print("TEST: m1 = m2 U m3")
  if (m1 == union m2 m3) 
    then print("=> PASSED")
    else print("=> FAILED")
  
  writeMSet "anag-out.txt" m1
  writeMSet "gana-out.txt" m4 