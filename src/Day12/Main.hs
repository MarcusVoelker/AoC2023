module Day12.Main where

import Data.List
import Data.Function
import Harness
import Text.Parsec
import Data.MemoTrie
import GHC.Generics (Generic)

fs :: Stream s m Char => ParsecT s u m [(String,[Int])]
fs = many1 $ do
  s <- many1 (char '.' <|> char '#' <|> char '?')
  spaces
  ds <- sepBy1 (read <$> many1 digit) (char ',')
  spaces
  return (s,map fromIntegral ds)

data IState = Run | Space | Wait deriving (Eq,Show, Generic)

instance HasTrie IState where
  newtype (IState :->: a) = IStateTrie { unIStateTrie :: Reg IState :->: a }
  trie = trieGeneric IStateTrie
  untrie = untrieGeneric unIStateTrie
  enumerate = enumerateGeneric unIStateTrie

memoFix3 :: (HasTrie a, HasTrie b, HasTrie c) => ((a -> b -> c -> d) -> a -> b -> c -> d) -> a -> b -> c -> d
memoFix3 f = fix (memo3 . f)

mFitCount :: String -> [Int] -> Int
mFitCount s i = memoFix3 fC s i Wait
  where
    check xs is r = if length (filter (== '#') xs) <= sum is && sum is <= length (filter (`elem` "#?") xs) then r else 0
    wait f xs is = check xs is $ f xs is Wait
    runE f xs is = check xs is $ f xs is Space
    runC f xs i is = check xs (i-1:is) $ f xs (i-1:is) Run
    runEC f xs i is | i == 1 = runE f xs is
                    | otherwise = runC f xs i is
    fC _ [] [] s | s /= Run = 1
    fC _ [] _ _ = 0
    fC _ xs [] Space = if '#' `notElem` xs then 1 else 0
    fC _ xs [] Wait = if '#' `notElem` xs then 1 else 0
    fC _ xs [] Run = 0
    fC f ('?':xs) is Space = wait f xs is
    fC f ('?':xs) (i:is) Run = runEC f xs i is
    fC f ('?':xs) (i:is) Wait = wait f xs (i:is) + runEC f xs i is
    fC _ ('#':xs) _ Space = 0
    fC f ('#':xs) (i:is) _ = runEC f xs i is
    fC _ ('.':xs) _ Run = 0
    fC f ('.':xs) is _ = wait f xs is

unfold :: (String,[Int]) -> (String,[Int])
unfold (s,i) = (intercalate "?" [s,s,s,s,s],i++i++i++i++i)

p1 :: [(String,[Int])] -> Int
p1 s = sum $ map (uncurry mFitCount) s

p2 :: [(String,[Int])] -> Int
p2 s = sum $ map (uncurry mFitCount . unfold) s

main :: Bool -> IO ()
main b = runParse 12 b fs p1 p2