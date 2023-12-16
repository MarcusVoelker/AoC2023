module Day7.Main where

import Data.Char
import Data.List
import Data.Ord
import Harness

data HandType = High | One | Two | Three | Full | Four | Five deriving (Eq, Show, Ord)

handType :: String -> HandType
handType s =
  let so = group $ sort s
   in case length so of
        1 -> Five
        2 -> if length (head so) `elem` [2, 3] then Full else Four
        3 -> case sort (map length so) of
          [1, 1, 3] -> Three
          [1, 2, 2] -> Two
        4 -> One
        5 -> High

repJ :: String -> Char -> String
repJ ('J' : r) c = c : repJ r c
repJ (h : r) c = h : repJ r c
repJ [] _ = []

handType2 :: String -> HandType
handType2 s = maximum $ map (handType . repJ s) "23456789TQKA"

charNum :: Char -> Int
charNum 'A' = 14
charNum 'K' = 13
charNum 'Q' = 12
charNum 'J' = 11
charNum 'T' = 10
charNum c = digitToInt c

charComp :: Char -> Char -> Ordering
charComp = comparing charNum

charNum2 :: Char -> Int
charNum2 'J' = 0
charNum2 c = charNum c

charComp2 :: Char -> Char -> Ordering
charComp2 = comparing charNum2

lexCompBy :: (a -> a -> Ordering) -> [a] -> [a] -> Ordering
lexCompBy o [] [] = EQ
lexCompBy o (c1 : l1) (c2 : l2)
  | o c1 c2 == EQ = lexCompBy o l1 l2
  | otherwise = o c1 c2

handSort :: (String -> HandType) -> (Char -> Char -> Ordering) -> String -> String -> Ordering
handSort t c h1 h2
  | t h1 /= t h2 = compare (t h1) (t h2)
  | otherwise = lexCompBy c h1 h2

p1 :: String -> Integer
p1 s =
  let dat = map (\l -> (take 5 l, read $ drop 6 l)) $ lines s
      sdat = sortBy (\a b -> handSort handType charComp (fst a) (fst b)) dat
   in sum $ zipWith (\s (_, n) -> s * n) [1 ..] sdat

p2 :: String -> Integer
p2 s =
  let dat = map (\l -> (take 5 l, read $ drop 6 l)) $ lines s
      sdat = sortBy (\a b -> handSort handType2 charComp2 (fst a) (fst b)) dat
   in sum $ zipWith (\s (_, n) -> s * n) [1 ..] sdat

main :: Bool -> IO ()
main b = run 7 b p1 p2