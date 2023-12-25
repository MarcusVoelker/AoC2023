module Day17.Main where

import Harness
import Data.List
import System.IO.Unsafe
import Data.Char
import Control.Monad
import qualified Data.Map as M
import Data.Graph.AStar
import Data.Hashable
import qualified Data.HashSet as S

data Dir = N | U | D | L | R deriving (Eq, Ord)

instance Hashable Dir where
  hashWithSalt s N = hashWithSalt s (0::Int)
  hashWithSalt s U = hashWithSalt s (1::Int)
  hashWithSalt s D = hashWithSalt s (2::Int)
  hashWithSalt s L = hashWithSalt s (3::Int)
  hashWithSalt s R = hashWithSalt s (4::Int)

instance Show Dir where
  show N = "O"
  show U = "^"
  show D = "v"
  show L = "<"
  show R = ">"

data Vertex = Vertex { pos :: !(Int, Int), dir :: !Dir,  streak :: !Int } deriving (Eq,Ord)

instance Hashable Vertex where
  hashWithSalt s (Vertex (y,x) d st) = hashWithSalt s (y,x,d,st)

instance Show Vertex where
  show (Vertex (y,x) d s) = show (x,y) ++ " in " ++ show d ++ "*" ++ show s

addDir :: Dir -> (Int,Int) -> (Int,Int)
addDir N (y,x) = (y,x)
addDir U (y,x) = (y-1,x)
addDir D (y,x) = (y+1,x)
addDir L (y,x) = (y,x-1)
addDir R (y,x) = (y,x+1)

unreturn :: Dir -> Dir -> Bool
unreturn U d = d /= D
unreturn R d = d /= L
unreturn D d = d /= U
unreturn L d = d /= R
unreturn N _ = True

rot :: Dir -> Dir -> Bool
rot U d = d == L || d == R
rot R d = d == U || d == D
rot D d = d == L || d == R
rot L d = d == U || d == D
rot N _ = True

successors :: (Int,Int) -> M.Map (Int,Int) Int -> Vertex -> S.HashSet Vertex
successors (h,w) grid v@(Vertex (y,x) d s) = S.fromList $ do
  d' <- [U,D,L,R]
  let (y',x') = addDir d' (y,x)
  guard $ y' >= 0 && x' >= 0 && y' < h && x' < w
  guard $ (s < 3 && unreturn d d') || (s == 3 && rot d d')
  return $ Vertex (y',x') d' (if d /= d' then 1 else s+1)

successors2 :: (Int,Int) -> M.Map (Int,Int) Int -> Vertex -> S.HashSet Vertex
successors2 (h,w) grid v@(Vertex (y,x) d s) = S.fromList $ do
  d' <- [U,D,L,R]
  let (y',x') = addDir d' (y,x)
  guard $ y' >= 0 && x' >= 0 && y' < h && x' < w
  guard $ (s < 4 && (d == d' || d == N)) || (4 <= s && s <= 10 && (rot d d' || d == d'))
  return $ Vertex (y',x') d' (if d /= d' then 1 else s+1)

dist :: M.Map (Int,Int) Int -> Vertex -> Vertex -> Int
dist m _ v = m M.! pos v

guess :: (Int,Int) -> M.Map (Int,Int) Int -> Vertex -> Int
guess (h,w) grid v = grid M.! pos v + (h - 1 - fst (pos v)) + (w - 1 - snd (pos v))

goal :: (Int,Int) -> Vertex -> Bool
goal (h,w) v = pos v == (h-1,w-1)

goal2 :: (Int,Int) -> Vertex -> Bool
goal2 (h,w) v = pos v == (h-1,w-1) && streak v >= 4

render :: (Int,Int) -> M.Map (Int,Int) Int -> [Vertex] -> IO ()
render (h,w) grid path = do
  forM_ [0..h-1] $ \y -> do
    forM_ [0..w-1] $ \x -> do
      if any (\v -> pos v == (y,x)) path then
        putStr $ show $ dir $ head (filter (\v -> pos v == (y,x)) path)
      else
        putStr $ show $ grid M.!(y,x)
    putStrLn ""

pathCost :: M.Map (Int,Int) Int -> [Vertex] -> Int
pathCost grid path = sum $ map (\v -> grid M.! pos v) path

p1 :: String -> Int
p1 s = let
  lgrid = map (map (\c -> ord c - ord '0')) $ lines s
  grid = M.fromList $ concat $ zipWith (\y l -> zipWith (\x c -> ((y,x),c)) [0..] l) [0..] lgrid
  bounds = (length lgrid, length (head lgrid))
  Just path = aStar (successors bounds grid) (dist grid) (guess bounds grid) (goal bounds) (Vertex (0,0) N 0)
  in unsafePerformIO (render (length lgrid, length (head lgrid)) grid path >> return (pathCost grid path))

p2 :: String -> Int
p2 s = let
  lgrid = map (map (\c -> ord c - ord '0')) $ lines s
  grid = M.fromList $ concat $ zipWith (\y l -> zipWith (\x c -> ((y,x),c)) [0..] l) [0..] lgrid
  bounds = (length lgrid, length (head lgrid))
  Just path = aStar (successors2 bounds grid) (dist grid) (guess bounds grid) (goal2 bounds) (Vertex (0,0) N 0)
  in unsafePerformIO (render (length lgrid, length (head lgrid)) grid path >> return (pathCost grid path))

main :: Bool -> IO ()
main b = run 17 b p1 p2