module Day4.Main where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Harness
import Text.Parsec
import Text.Parsec.Token

type Card = (Int, [Int], [Int])

card :: Stream s m Char => ParsecT s u m Card
card = do
  string "Card"
  many1 space
  i <- read <$> many1 digit
  string ":"
  many space
  a <- many1 (do d <- read <$> many1 digit; many1 space; return d)
  many space
  string "|"
  many space
  b <- sepBy1 (read <$> many1 digit) (many1 (string " "))
  return (i, a, b)

cards :: Stream s m Char => ParsecT s u m [Card]
cards = sepBy1 card (many space)

p1 :: [Card] -> Int
p1 cs = sum $ [2 ^ (l -1) | (i, a, b) <- cs, let l = length (a `intersect` b), l > 0]

addCards :: M.Map Int Int -> (Int, Int) -> M.Map Int Int
addCards m (i, c) = foldr (M.adjust (+ (m M.! i))) m [i + 1 .. i + c]

p2 :: [Card] -> Int
p2 cs =
  let ns = map (\(i, a, b) -> (i, length (a `intersect` b))) cs
   in M.foldr (+) 0 $ foldl addCards (M.fromList (map (\(i, _) -> (i, 1)) ns)) ns

main :: Bool -> IO ()
main b = runParse 4 b cards p1 p2