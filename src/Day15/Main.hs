module Day15.Main where

import Harness
import Data.Char
import qualified Data.Map as M

step :: Int -> Char -> Int
step v c = mod ((ord c + v)*17) 256

separate :: String -> [String]
separate [] = [[]]
separate (',':s) = []:separate s
separate (c:s) = (\(x:xs) -> (c:x):xs) $ separate s

hash :: String -> Int
hash = foldl step 0

p1 :: String -> Int
p1 s = sum $ map hash $ separate s

data Instr = Set String Int | Take String deriving Show

parse :: String -> Instr
parse s | last s == '-' = Take $ init s
parse s = let
  (a,b) = break (== '=') s
  in Set a (read $ tail b)

execute :: M.Map Int [(String,Int)] -> Instr -> M.Map Int [(String,Int)]
execute m (Set s i) = let
  h = hash s
  in M.insertWith (\_ a -> if all ((/=s).fst) a then a++[(s,i)] else (\(a,b) -> a ++ ((s,i):tail b)) $ break ((==s).fst) a) h [(s,i)] m
execute m (Take s) = let
  h = hash s
  in M.adjust (filter ((/= s).fst)) h m

power :: M.Map Int [(String,Int)] -> Int
power = M.foldrWithKey (\b l s -> s + (b+1) * sum (zipWith (*) [1..] $ map snd l)) 0

p2 :: String -> Int
p2 s = power $ foldl execute M.empty $ map parse $ separate s

main :: Bool -> IO ()
main b = run 15 b p1 p2