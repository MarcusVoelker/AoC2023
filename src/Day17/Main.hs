module Day17.Main where

import Harness
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Data.List
import System.IO.Unsafe
import Data.Char
import qualified Data.Map as M

data Dir = N | U | D | L | R deriving (Eq, Ord)

instance Show Dir where
  show N = "O"
  show U = "^"
  show D = "v"
  show L = "<"
  show R = ">"

data Vertex = Vertex { _pos :: !(Int, Int), _dir :: !Dir,  _streak :: !Int, _cost :: !Int, _predec :: Maybe Vertex }

makeLenses ''Vertex

instance Eq Vertex where
  (==) a b = (a^.pos) == (b^.pos) && (a^.dir) == (b^.dir) && (a^.streak) == (b^.streak)

instance Show Vertex where
  show (Vertex (y,x) d s c _) = show (x,y) ++ " in " ++ show d ++ "*" ++ show s ++ ", cost:" ++ show c

data PathState = PathState { _open :: ![Vertex], _closed :: ![Vertex] } deriving (Eq, Show)

makeLenses ''PathState

getPath :: Maybe Vertex -> State PathState [Vertex]
getPath Nothing = return []
getPath (Just v) = do
  p <- getPath $ v^.predec
  return $ p++[v]

astar :: (Int,Int) -> M.Map (Int,Int) Int -> [Vertex]
astar (h,w) grid = evalState (do
  open %= (Vertex (0,0) N 0 0 Nothing :)
  lv <- lastM (not . null <$> use open) $ do
    cur <- open %%= (\x -> (head x, tail x))
    o <- use open
    if cur^.pos == (h-1,w-1) then do
      open .= []
      return $ Just cur
    else do
      closed %= (cur :)
      expand (h,w) grid cur
      return Nothing
  getPath (join lv)) $ PathState [] []

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

successors :: (Int,Int) -> M.Map (Int,Int) Int -> Vertex -> [Vertex]
successors (h,w) grid v@(Vertex (y,x) d s c _) = do
  d' <- [U,D,L,R]
  let (y',x') = addDir d' (y,x)
  guard $ y' >= 0 && x' >= 0 && y' < h && x' < w
  guard $ (s < 3 && unreturn d d') || (s == 3 && rot d d')
  return $ Vertex (y',x') d' (if d /= d' then 1 else s+1) (c + grid M.! (y',x')) (Just v)

guess :: (Int,Int) -> M.Map (Int,Int) Int -> Vertex -> Int
guess (h,w) grid v = v^.cost + (h - 1 - fst (v^.pos)) + (w - 1 - snd (v^.pos))

expand :: (Int,Int) -> M.Map (Int,Int) Int -> Vertex -> State PathState ()
expand b grid cur = do
  let succs = successors b grid cur
  forM_ succs $ \s -> do
    unseen <- not . (s `elem`) <$> use closed
    unopen <- not . any (\v -> v == s && v^.cost <= s^.cost) <$> use open
    when (unseen && unopen) $ do
      open %= filter (/= s)
      open %= (s :)
  open %= sortOn (guess b grid)

render :: (Int,Int) -> M.Map (Int,Int) Int -> [Vertex] -> IO ()
render (h,w) grid path = do
  forM_ [0..h-1] $ \y -> do
    forM_ [0..w-1] $ \x -> do
      if any (\v -> v^.pos == (y,x)) path then
        putStr $ show $ head (filter (\v -> v^.pos == (y,x)) path) ^. dir
      else
        putStr $ show $ grid M.!(y,x)
    putStrLn ""

p1 :: String -> Int
p1 s = let
  lgrid = map (map (\c -> ord c - ord '0')) $ lines s
  grid = M.fromList $ concat $ zipWith (\y l -> zipWith (\x c -> ((y,x),c)) [0..] l) [0..] lgrid
  path = astar (length lgrid, length (head lgrid)) grid
  in unsafePerformIO (render (length lgrid, length (head lgrid)) grid path >> return (last path^.cost))

p2 :: String -> Int
p2 _ = 0

main :: Bool -> IO ()
main b = run 17 b p1 p2