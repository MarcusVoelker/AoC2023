module Day9.Main where

import Harness
import Text.Parsec

diffs :: [Int] -> [Int]
diffs (x:y:xs) = (y - x) : diffs (y:xs)
diffs _ = []

extend :: [Int] -> Int
extend xs | all (==0) xs = 0
  | otherwise = let
    dx = diffs xs
    dxe = extend dx
    in last xs + dxe

prepend :: [Int] -> Int
prepend xs | all (==0) xs = 0
  | otherwise = let
    dx = diffs xs
    dxe = prepend dx
    in head xs - dxe

p1 :: [[Int]] -> Int
p1 = sum . map extend

p2 :: [[Int]] -> Int
p2 = sum . map prepend

intll :: Stream s m Char => ParsecT s u m [[Int]]
intll = many1 $ do
  l <- sepBy1 (read <$> ((++) <$> option "" (string "-") <*> many1 digit)) (string " ")
  char '\n'
  return l

main :: Bool -> IO ()
main b = runParse 9 b intll p1 p2