module Day3.Main where

import Control.Monad
import Data.Char
import Data.List
import Harness

data Cell = Empty | Cell {op :: Char} | Part {line :: Int, x0 :: Int, x1 :: Int, n :: Int} deriving (Eq)

isPart :: Cell -> Bool
isPart Part {} = True
isPart _ = False

instance Show Cell where
  show Empty = "."
  show (Cell c) = [c]
  show (Part l x0 x1 n) = show n ++ "@(" ++ show l ++ ":" ++ show x0 ++ "-" ++ show x1 ++ ")"

cellZipper :: ((Int, Int), Char) -> ((Int, Int), Char) -> ((Int, Int), Char) -> ((Int, Int), Char) -> ((Int, Int), Char) -> Cell
cellZipper (_, a) (_, b) ((x, y), c) (_, d) (_, e)
  | isDigit c =
      if isDigit b
        then
          if isDigit a
            then Part y (x - 2) x (read [a, b, c])
            else
              if isDigit d
                then Part y (x - 1) (x + 1) (read [b, c, d])
                else Part y (x - 1) x (read [b, c])
        else
          if isDigit d
            then
              if isDigit e
                then Part y x (x + 2) (read [c, d, e])
                else Part y x (x + 1) (read [c, d])
            else Part y x x (read [c])
  | c == '.' = Empty
  | otherwise = Cell c

vCell :: ((Int, Int), Char)
vCell = ((-1, -1), '.')

neighbors :: String -> [[Cell]]
neighbors ss =
  let ant = zipWith (\y l -> map (\(x, c) -> ((x, y), c)) l) [0 ..] $ map (zip [0 ..]) $ lines ss
   in map (\l -> zipWith5 cellZipper (vCell : vCell : l) (vCell : l) l (tail l ++ [vCell]) (tail (tail l) ++ [vCell, vCell])) ant

neighCollect :: (Cell, Cell, Cell) -> (Cell, Cell, Cell) -> (Cell, Cell, Cell) -> [Cell]
neighCollect (a, b, c) (d, Cell e, f) (g, h, i) = filter isPart [a, b, c, d, f, g, h, i]
neighCollect _ _ _ = []

collect :: [[Cell]] -> [Cell]
collect cs =
  let hori = map (\l -> zip3 (Empty : l) l (tail l ++ [Empty])) cs
      neigh = zipWith3 (zipWith3 neighCollect) (repeat (Empty, Empty, Empty) : hori) hori (tail hori ++ [repeat (Empty, Empty, Empty)])
   in nub (join (join neigh))

neighRatio :: (Cell, Cell, Cell) -> (Cell, Cell, Cell) -> (Cell, Cell, Cell) -> [Int]
neighRatio (a, b, c) (d, Cell '*', f) (g, h, i)
  | length (filter isPart (nub [a, b, c, d, f, g, h, i])) == 2 = return $ (\[a, b] -> n a * n b) $ filter isPart (nub [a, b, c, d, f, g, h, i])
neighRatio _ _ _ = []

ratio :: [[Cell]] -> [Int]
ratio cs =
  let hori = map (\l -> zip3 (Empty : l) l (tail l ++ [Empty])) cs
      neigh = zipWith3 (zipWith3 neighRatio) (repeat (Empty, Empty, Empty) : hori) hori (tail hori ++ [repeat (Empty, Empty, Empty)])
   in join (join neigh)

p1 :: String -> Int
p1 ss = sum $ map (\(Part _ _ _ n) -> n) $ collect (neighbors ss)

p2 :: String -> Int
p2 ss = sum $ ratio (neighbors ss)

main :: Bool -> IO ()
main b = run 3 b p1 p2