module Day21.Main where

import Harness
import Control.Monad
import Data.List
import qualified Data.Set as S

free :: [String] -> S.Set (Int,Int)
free s = S.fromList $ do
  (y, l) <- zip [0..] s
  (x, c) <- zip [0..] l
  guard $ c == '.'
  return (x,y)

freeInf :: [String] -> ((Int,Int) -> Bool)
freeInf s = let set = S.fromList $ do
                  (y, l) <- zip [0..] s
                  (x, c) <- zip [0..] l
                  guard $ c == '.'
                  return (x,y)
  in
    \(x,y) -> let x' = x `mod` length (head s); y' = y `mod` length s in S.member (x',y') set

start :: [String] -> (Int,Int)
start s = let
  (y, l) = head $ filter (\(_,l) -> 'S' `elem` l) $ zip [0..] s
  (x, _) = head $ filter (\(_,c) -> c == 'S') $ zip [0..] l
  in (x,y)

step :: S.Set (Int,Int) -> ([(Int,Int)],S.Set (Int,Int)) -> ([(Int,Int)],S.Set (Int,Int))
step free (open,closed) = let
  open' = nub $ do
    (x,y) <- open
    (x',y') <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    guard $ S.notMember (x',y') closed
    guard $ S.member (x',y') free
    return (x',y')
  closed' = S.union closed $ S.fromList open
  in (open', closed')

stepP :: ((Int,Int) -> Bool) -> ([(Int,Int)],S.Set (Int,Int)) -> ([(Int,Int)],S.Set (Int,Int))
stepP free (open,closed) = let
  open' = nub $ do
    (x,y) <- open
    (x',y') <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    guard $ S.notMember (x',y') closed
    guard $ free (x',y')
    return (x',y')
  closed' = S.union closed $ S.fromList open
  in (open', closed')

reach :: S.Set (Int,Int) -> (Int,Int) -> Int -> S.Set (Int,Int)
reach free p n = (\(b,c) -> S.union c $ S.fromList b) $ iterate (step free) ([p],S.empty) !! n

reachP :: ((Int,Int) -> Bool) -> (Int,Int) -> Int -> S.Set (Int,Int)
reachP free p n = (\(b,c) -> S.union c $ S.fromList b) $ iterate (stepP free) ([p],S.empty) !! n

pEq :: (Int,Int) -> (Int,Int) -> Bool
pEq (x,y) (x',y') = mod (x+y) 2 == mod (x'+y') 2

pNeq :: (Int,Int) -> (Int,Int) -> Bool
pNeq p p' = not $ pEq p p'

p1 :: String -> Int
p1 s = let
  frees = free $ lines s
  st = start $ lines s
  in S.size $ S.filter (`pEq` st) $ reach frees st 64

p2 :: String -> (Int,Int,Int)
p2 s = let
  frees = freeInf $ lines s
  st = start $ lines s
  s0 = S.size $ S.filter (`pNeq` st) $ reachP frees st 65
  s1 = S.size $ S.filter (`pEq` st) $ reachP frees st (65+131)
  s2 = S.size $ S.filter (`pNeq` st) $ reachP frees st (65+131*2)
  in (s0,s1,s2)

main :: Bool -> IO ()
main b = run 21 b p1 p2