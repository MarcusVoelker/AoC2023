module Day13.Main where

import Harness
import Text.Parsec
import Data.List
import Data.Maybe
import Data.Bifunctor
import System.IO.Unsafe

split :: String -> [[String]]
split s = let
  split' [] = [[]]
  split' ("":xs) = []:split' xs
  split' (l:xs) = (\(r:rs) -> (l:r):rs)$ split' xs
  in split' $ lines s

splits :: [a] -> [([a],[a])]
splits [x,y] = [([x],[y])]
splits (x:xs) = ([x],xs) : map (first (x:)) (splits xs)

overlaps :: (Eq a) => [a] -> [a] -> Bool
overlaps l r = all (uncurry (==)) $ zip (reverse l) r

refl :: [String] -> Maybe Int
refl xs = listToMaybe $ refls xs

refls :: [String] -> [Int]
refls xs = let
  vert = map fst $ filter (\(_,(l,r)) -> overlaps l r) $ zip [100,200..] $ splits xs
  hori = map fst $ filter (\(_,(l,r)) -> overlaps l r) $ zip [1..] $ splits $ transpose xs
  in (vert ++ hori)

smudge :: [String] -> [[String]]
smudge x = do
  a <- [0..length x-1]
  b <- [0..length (x!!a)-1]
  let (u,y:d) = splitAt a x
  let (l,c:r) = splitAt b y
  return $ u ++ [l ++ [if c == '.' then '#' else '.'] ++ r] ++ d

srefl :: [String] -> Int
srefl xs = let
  res = filter (/= fromJust (refl xs)) $ concatMap refls $ smudge xs
  in if null res then
    unsafePerformIO $ print xs >> return 0
  else
    head res

p1 :: String -> Int
p1 s = sum $ mapMaybe refl $ split s

p2 :: String -> Int
p2 s = sum $ map srefl $ split s

main :: Bool -> IO ()
main b = run 13 b p1 p2