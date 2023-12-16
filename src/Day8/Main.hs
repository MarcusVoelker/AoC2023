module Day8.Main where

import Control.Monad
import Data.List
import qualified Data.Map as M
import Harness
import Text.Parsec
import Text.Parsec.Token

instr :: (Stream s m Char) => ParsecT s u m ([Bool], M.Map String (String, String))
instr = do
  l <- many1 ((char 'L' >> return False) <|> (char 'R' >> return True))
  spaces
  ss <- many1 $ do
    s1 <- many1 letter
    string " = ("
    s2 <- many1 letter
    string ", "
    s3 <- many1 letter
    string ")"
    many space
    return (s1, (s2, s3))
  return (l, M.fromList ss)

p1 :: ([Bool], M.Map String (String, String)) -> Int
p1 (is, rs) = length $ takeWhile (/= "ZZZ") $ scanl (\c b -> (if b then snd else fst) (rs M.! c)) "AAA" (cycle is)

p2 :: ([Bool], M.Map String (String, String)) -> Int
p2 (is, rs) = length $ takeWhile (any ((/= 'Z') . last)) $ scanl (\cs b -> cs >>= \c -> [(if b then snd else fst) (rs M.! c)]) (filter (\k -> last k == 'A') $ M.keys rs) (cycle is)

p1D :: ([Bool], M.Map String (String, String)) -> IO ()
p1D (is, rs) = mapM_ print $ scanl (\c b -> (if b then snd else fst) (rs M.! c)) "AAA" (cycle is)

orbit :: ([Bool], M.Map String (String, String)) -> (Int, String) -> [(Int, String)]
orbit (is, rs) (i0, s0) =
  let trans (idx, k) = (mod (idx + 1) (length is), (if is !! idx then snd else fst) (rs M.! k))
   in iterate trans (i0, s0)

p2D :: ([Bool], M.Map String (String, String)) -> IO ()
p2D (is, rs) = print $ orbit (is, rs) (0, "AAA")

main :: Bool -> IO ()
main b = runParse 8 b instr p1 p2

mainDebug :: Bool -> IO ()
mainDebug b = debugParse 8 b instr p1D p2D