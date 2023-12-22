module Day18.Main where

import Harness
import Text.Parsec
import Data.Char

data Dir = U | R | D | L deriving (Show,Eq)

data Instr = Instr Dir Integer String

instance Show Instr where
  show (Instr d i s) = show d ++ " " ++ show i ++ "(#" ++ s ++ ")"

instrs :: Stream s m Char => ParsecT s u m [Instr]
instrs = many1 $ do
  d <- oneOf "URDL"
  spaces
  i <- read <$> many1 digit
  spaces
  string "(#"
  s <- many1 (noneOf ")")
  string ")"
  spaces
  return $ Instr (case d of
    'U' -> U
    'R' -> R
    'D' -> D
    'L' -> L) i s

dir :: Char -> Dir
dir '0' = R
dir '1' = D
dir '2' = L
dir '3' = U

rot :: Dir -> Dir
rot U = R
rot R = D
rot D = L
rot L = U

irot :: Dir -> Dir
irot U = L
irot R = U
irot D = R
irot L = D

hex :: String -> Integer
hex [] = 0
hex (c:x) = toInteger (digitToInt c) + 16 * hex x

iFlip :: Instr -> Instr
iFlip (Instr _ _ s) = Instr (dir $ last s) (hex $ reverse $ init s) ""

step :: [(Int,Int)] -> Instr -> [(Int,Int)]
step l@((x,y):_) (Instr U i _) = map (x,) [y-fromIntegral i..y-1] ++ l
step l@((x,y):_) (Instr R i _) = map (,y) [x+fromIntegral i,x+fromIntegral i-1..x+1] ++ l
step l@((x,y):_) (Instr D i _) = map (x,) [y+fromIntegral i,y+fromIntegral i-1..y+1] ++ l
step l@((x,y):_) (Instr L i _) = map (,y) [x-fromIntegral i..x-1] ++ l

stepI :: (Dir,Integer,Integer) -> (Instr,Instr) -> (Dir, Integer,Integer)
stepI (d,h,a) (Instr U i _,Instr d' _ _) | d /= d' = (U,h+i+1,a)
                                         | d == d' = (D,h+i,a)
stepI (d,h,a) (Instr R i _,Instr d' _ _) | d /= d' = (R,h,a+h*(i+1))
                                         | d == d' = (L,h,a+h*i)
stepI (d,h,a) (Instr D i _,Instr d' _ _) | d /= d' = (D,h,a+h*(i+1))
                                         | d == d' = (U,h,a+h*i)
stepI (d,h,a) (Instr L i _,Instr d' _ _) | d /= d' = (L,h,a+h*(i+1))
                                         | d == d' = (R,h,a+h*i)

debSol :: [(Int,Int)] -> IO ()
debSol sol = let
  minX = minimum $ map fst sol
  maxX = maximum $ map fst sol
  minY = minimum $ map snd sol
  maxY = maximum $ map snd sol
  in mapM_ (\y -> putStrLn $ map (\x -> if (x,y) `elem` sol then '#' else '.') [minX..maxX]) [minY..maxY]

floodFill :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
floodFill closed [] = closed
floodFill closed ((x,y):open)
  | (x,y) `elem` closed = floodFill closed open
  | otherwise = floodFill ((x,y):closed) $ filter (\p -> p `notElem` closed && p `notElem` open) [(x+1,y),(x-1,y),(x,y+1),(x,y-1)] ++ open

p1 :: [Instr] -> Integer
p1 is = let
  sol = foldl step [(0,0)] is
  minX = minimum $ map fst sol
  maxX = maximum $ map fst sol
  minY = minimum $ map snd sol
  maxY = maximum $ map snd sol
  closed = filter (\(x,y) -> (x,y) `elem` sol) [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]
  open = map (\(x,y) -> (x,y+1)) $ filter (\(x,y) -> y == minY && (x,y+1) `notElem` sol) sol
  ls = floodFill closed open
  in debugOut (debSol ls) (fromIntegral $ length $ [1 | x <- [minX..maxX], y <- [minY..maxY], (x,y) `elem` ls])

p2 :: [Instr] -> Integer
p2 is = let 
  c1 = (\(_,_,x) -> x) $ foldl stepI ((\(Instr d _ _) -> rot d) $ head is,0,0) (zip is (tail is ++ [head is]))
  c2 = (\(_,_,x) -> x) $ foldl stepI ((\(Instr d _ _) -> irot d) $ head is,0,0) (zip is (tail is ++ [head is]))
  in max c1 c2

main :: Bool -> IO ()
main b = runParse 18 b instrs p1 p2

test :: [Instr]
test = [
  Instr R 6 "70c710",
  Instr D 5 "0dc571",
  Instr L 2 "5713f0",
  Instr D 2 "d2c081",
  Instr R 2 "59c680",
  Instr D 2 "411b91",
  Instr L 5 "8ceee2",
  Instr U 2 "caa173",
  Instr L 1 "1b58a2",
  Instr U 2 "caa171",
  Instr R 2 "7807d2",
  Instr U 3 "a77fa3",
  Instr L 2 "015232",
  Instr U 2 "7a21e3"]