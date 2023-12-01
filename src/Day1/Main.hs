module Day1.Main where

import Control.Monad
import Data.Char
import Data.List
import Harness

p1 :: String -> Int
p1 s = sum $ map (read . (\l -> [head l, last l]) . filter isDigit) $ lines s

p2 :: String -> Int
p2 s = sum [10 * head l + last l | l <- [[x | t <- tails v, t /= [], let x = sum [n | n <- [1 .. 9], c <- [["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !! (n -1), show n], c `isPrefixOf` t], x > 0] | v <- lines s]]

dp2 :: String -> Int
dp2 s = sum $ do
  l <- do
    v <- lines s
    return $ do
      t <- tails v
      guard $ t /= []
      let x = sum $ do
            n <- [1 .. 9]
            c <- [["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !! (n -1), show n]
            guard $ c `isPrefixOf` t
            return n
      guard $ x > 0
      return x
  return (10 * head l + last l)

main :: Bool -> IO ()
main b = run 1 b p1 p2