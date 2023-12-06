module Day6.Main where

import Harness

rawData :: [(Integer, Integer)]
rawData = [(44, 208), (80, 1581), (65, 1050), (72, 1102)]

rawData2 :: (Integer, Integer)
rawData2 = (44806572, 208158110501102)

count :: (Integer, Integer) -> Integer
count (a, b) = fromIntegral $ length $ filter (> b) $ map (\h -> h * (a - h)) [0 .. a]

goodCount :: (Integer, Integer) -> Integer
goodCount (a, b) =
  let fa = fromIntegral a
      fb = fromIntegral b
      ph = fa / 2
      min = floor (ph - sqrt (ph * ph - fb) + 1)
      max = ceiling (ph + sqrt (ph * ph - fb) - 1)
   in max - min + 1

p1 :: Integer
p1 = product $ map goodCount rawData

p2 :: Integer
p2 = goodCount rawData2

main :: Bool -> IO ()
main b = print $ if b then p2 else p1