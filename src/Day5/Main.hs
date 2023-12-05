module Day5.Main where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Harness
import Text.Parsec hiding (many, (<|>))
import Text.Parsec.Token

type AMap = Integer -> Integer

type IMap = (Integer, Integer) -> [(Integer, Integer)]

interpret :: Integer -> (Integer, Integer, Integer) -> Maybe Integer
interpret x (d, s, l)
  | s <= x && x < s + l = Just (x + (d - s))
  | otherwise = Nothing

makeMap :: [(Integer, Integer, Integer)] -> AMap
makeMap rs i = fromMaybe i (foldr1 (<|>) (map (interpret i) rs))

aMap :: (Stream s m Char) => ParsecT s u m [(Integer, Integer, Integer)]
aMap = do
  many (void letter <|> void (string "-"))
  space
  string "map:"
  space
  rs <-
    many1
      ( do
          n1 <- read <$> many1 digit
          space
          n2 <- read <$> many1 digit
          space
          n3 <- read <$> many1 digit
          space
          return (n1, n2, n3)
      )
  many space
  return rs

fullA :: (Stream s m Char) => ParsecT s u m ([Integer], [[(Integer, Integer, Integer)]])
fullA = do
  string "seeds: "
  ss <- sepBy1 (read <$> many1 digit) (string " ")
  many space
  ms <- many1 aMap
  return (ss, ms)

p1 :: ([Integer], [[(Integer, Integer, Integer)]]) -> Integer
p1 (ss, ms) = minimum (map (foldr1 (.) (reverse (map makeMap ms))) ss)

intinterpret :: (Integer, Integer, Integer) -> (Integer, Integer) -> [(Integer, Integer)]
intinterpret (d, s, w) (l, u)
  | u < s || l >= s + w = [(l, u)]
  | l < s && u >= s + w = [(l, s - 1), (d, d + w - 1), (s + w, u)]
  | l < s && u >= s && u < s + w = [(l, s - 1), (d, u + (d - s))]
  | l >= s && l < s + w && u >= s + w = [(l + (d - s), d + w - 1), (s + w, u)]
  | otherwise = [(l + (d - s), u + (d - s))]

makeIMap :: [(Integer, Integer, Integer)] -> IMap
makeIMap rs (l, u) = foldl (\is r -> is >>= intinterpret r) [(l, u)] rs

initials :: [Integer] -> [(Integer, Integer)]
initials (x : l : xs) = (x, x + l - 1) : initials xs
initials _ = []

intervaluate :: [[(Integer, Integer, Integer)]] -> [(Integer, Integer)] -> [(Integer, Integer)]
intervaluate ms ins =
  let ims = map makeIMap ms
   in ins >>= (\i -> foldM (\int mp -> mp int) i ims)

extract :: [(Integer, Integer)] -> Integer
extract = minimum . map fst

p2 :: ([Integer], [[(Integer, Integer, Integer)]]) -> Integer
p2 (ss, ms) = extract $ intervaluate ms (initials ss)

main :: Bool -> IO ()
main b = runParse 5 b fullA p1 p2

teststring =
  unlines
    [ "seeds: 79 14 55 13",
      "",
      "seed-to-soil map:",
      "50 98 2",
      "52 50 48",
      "",
      "soil-to-fertilizer map:",
      "0 15 37",
      "37 52 2",
      "39 0 15",
      "",
      "fertilizer-to-water map:",
      "49 53 8",
      "0 11 42",
      "42 0 7",
      "57 7 4",
      "",
      "water-to-light map:",
      "88 18 7",
      "18 25 70",
      "",
      "light-to-temperature map:",
      "45 77 23",
      "81 45 19",
      "68 64 13",
      "",
      "temperature-to-humidity map:",
      "0 69 1",
      "1 0 69",
      "",
      "humidity-to-location map:",
      "60 56 37",
      "56 93 4"
    ]