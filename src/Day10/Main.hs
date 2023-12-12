module Day10.Main where

import Harness
import qualified Data.Map as M

data Dir = L | R | U | D deriving (Show,Eq)

dir :: Dir -> (Int,Int) -> (Int,Int)
dir U (x,y) = (x,y-1)
dir R (x,y) = (x+1,y)
dir D (x,y) = (x,y+1)
dir L (x,y) = (x-1,y)

pathLength :: M.Map (Int,Int) Char -> (Int,Int) -> Dir -> Int
pathLength g s d = let
  maplook p = M.findWithDefault '.' p g
  pathLength' p d | s == p = 0
    | otherwise =
      let d' = case (g M.! p,d) of
            ('|',U) -> U
            ('|',D) -> D
            ('-',R) -> R
            ('-',L) -> L
            ('L',D) -> R
            ('L',L) -> U
            ('J',D) -> L
            ('J',R) -> U
            ('F',U) -> R
            ('F',L) -> D
            ('7',U) -> L
            ('7',R) -> D
      in 1 + pathLength' (dir d' p) d'
  in 1 + pathLength' (dir d s) d

pathCollect :: M.Map (Int,Int) Char -> (Int,Int) -> Dir -> [((Int,Int),Char)]
pathCollect g s d = let
  maplook p = M.findWithDefault '.' p g
  pathCollect' p d | s == p = []
    | otherwise =
      let d' = case (g M.! p,d) of
            ('|',U) -> U
            ('|',D) -> D
            ('-',R) -> R
            ('-',L) -> L
            ('L',D) -> R
            ('L',L) -> U
            ('J',D) -> L
            ('J',R) -> U
            ('F',U) -> R
            ('F',L) -> D
            ('7',U) -> L
            ('7',R) -> D
      in (p,g M.! p) : pathCollect' (dir d' p) d'
  in (s, '-'): pathCollect' (dir d s) d

p1 :: String -> Int
p1 s = let
  idxed = M.fromList $ concat $ zipWith (\i l -> zipWith (\j c -> ((j,i),c)) [0..] l) [0..] $ lines s
  ((sx,sy),_) = head $ M.toList $ M.filter (== 'S') idxed
  maplook p = M.findWithDefault '.' p idxed
  leftS = maplook (sx-1,sy) `elem` "-FL"
  rightS = maplook (sx+1,sy) `elem` "-J7"
  upS = maplook (sx,sy-1) `elem` "|F7"
  downS = maplook (sx,sy+1) `elem` "|LJ"
  in div (pathLength idxed (sx,sy) R) 2

p2 :: String -> Int
p2 _ = 0

main :: Bool -> IO ()
main b = run 10 b p1 p2

dp1 :: String -> IO ()
dp1 s = do
  let idxed = M.fromList $ concat $ zipWith (\i l -> zipWith (\j c -> ((j,i),c)) [0..] l) [0..] $ lines s
  let ((sx,sy),_) = head $ M.toList $ M.filter (== 'S') idxed
  let maplook p = M.findWithDefault '.' p idxed
  let leftS = maplook (sx-1,sy) `elem` "-FL"
  let rightS = maplook (sx+1,sy) `elem` "-J7"
  let upS = maplook (sx,sy-1) `elem` "|F7"
  let downS = maplook (sx,sy+1) `elem` "|LJ"
  print $ div (pathLength idxed (sx,sy) R) 2

inCheck :: M.Map (Int,Int) Char -> (Int,Int) -> Bool
inCheck g (x,y) | M.member (x,y) g = False
  | otherwise = countUp y
  where countUp y | y < 0 = False
                  | otherwise = (if M.findWithDefault '.' (x,y) g `elem` "-J7" then not else id) $ countUp (y-1)

dp2 :: String -> IO ()
dp2 s = do
  let idxed = M.fromList $ concat $ zipWith (\i l -> zipWith (\j c -> ((j,i),c)) [0..] l) [0..] $ lines s
  let ((sx,sy),_) = head $ M.toList $ M.filter (== 'S') idxed
  let p = pathCollect idxed (sx,sy) R
  let cands = [(x,y) | x <- [minimum (map (fst . fst) p)..maximum (map (fst . fst) p)], y <- [minimum (map (snd . fst) p)..maximum (map (snd . fst) p)]]
  let g = M.fromList p
  print $ length $ filter (inCheck g) cands


mainDebug :: Bool -> IO ()
mainDebug b = runIO 10 b dp1 dp2