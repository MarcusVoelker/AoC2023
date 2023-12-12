module Day11.Main where

import Data.List
import Control.Monad
import Harness

p1 :: String -> Int
p1 s = let
  ls = lines s >>= (\l -> if all (== '.') l then [l,l] else [l])
  ls' = transpose (transpose ls >>= (\l -> if all (== '.') l then [l,l] else [l]))
  z = zipWith (\i l -> zipWith (\j c -> ((i,j),c)) [0..] l) [0..] ls'
  ps = map fst $ filter ((=='#'). snd) (join z)
  ds = do
    (x,y) <- ps
    (x',y') <- ps
    guard $ (x,y) < (x',y')
    [abs (x-x') + abs (y-y')]
  in sum ds

p2 :: String -> Int
p2 s = let
  rs = join (zipWith (\z l -> if all (== '.') l then [z] else []) [0..] (lines s))
  cs = join (zipWith (\z l -> if all (== '.') l then [z] else []) [0..] (transpose $ lines s))
  z = zipWith (\y l -> zipWith (\x c -> ((x,y),c)) [0..] l) [0..] $ lines s
  ps = map fst $ filter ((=='#'). snd) (join z)
  ds = do
    (x,y) <- ps
    (x',y') <- ps
    guard $ (x,y) < (x',y')
    let dx = length $ filter (\c -> min x x' < c && c < max x x') cs
    let dy = length $ filter (\r -> min y y' < r && r < max y y') rs
    [abs (x-x') + 999999*dx + abs (y-y') + 999999*dy]
  in sum ds

main :: Bool -> IO ()
main b = run 11 b p1 p2