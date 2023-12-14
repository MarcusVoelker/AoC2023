module Day14.Main where

import Harness

import Data.List
import System.IO.Unsafe
import Data.MemoTrie

stepRock :: Char -> Char -> (Char,Char)
stepRock '.' 'O' = ('O','.')
stepRock '.' c = ('.',c)
stepRock 'O' c = ('O',c)
stepRock '#' c = ('#',c)

moveRocks :: String -> String -> (String,String)
moveRocks xs ys = unzip $ zipWith stepRock xs ys

tilt1 :: [String] -> [String]
tilt1 (a:b:ss) = let
  (a',b') = moveRocks a b
  in a':tilt1 (b':ss)
tilt1 s = s

tiltN :: [String] -> [String]
tiltN s = let
  s' = tilt1 s
  in if s == s' then s else tiltN s'

tiltS :: [String] -> [String]
tiltS = reverse . tiltN . reverse

tiltW :: [String] -> [String]
tiltW = transpose . tiltN . transpose

tiltE :: [String] -> [String]
tiltE = transpose . reverse . tiltN . reverse . transpose

spin :: [String] -> [String]
spin = tiltE . tiltS . tiltW . tiltN

score :: [String] -> Int
score s = sum $ zipWith (\i b -> length (filter (== 'O') b) * i) [length s,length s-1..1] s

p1 :: String -> Int
p1 s = score $ tiltN $ lines s

-- this does NOT calculate the solution, that was done using the iteration in the REPL with additional manual arithmetic
p2 :: String -> Int
p2 s = score $ head $ drop 3 $ iterate (memo spin) $ lines s

main :: Bool -> IO ()
main b = run 14 b p1 p2