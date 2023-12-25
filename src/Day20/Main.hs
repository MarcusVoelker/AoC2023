module Day20.Main where

import Harness

import Control.Monad
import Text.Parsec
import Data.List
import System.IO.Unsafe
import qualified Data.Map as M

data Module = Broadcast { targets :: [String] }
            | FlipFlop { state :: Bool, targets :: [String] }
            | Conjunction { memory :: M.Map String Bool, targets :: [String] }

instance Show Module where
  show (Broadcast ts) = "broadcaster -> " ++ intercalate "," (map show ts)
  show (FlipFlop q ts) = "%" ++ show q ++ " -> " ++ intercalate "," (map show ts)
  show (Conjunction m ts) = "&" ++ show m ++ " " ++ intercalate "," (map show ts)

type Circuit = M.Map String Module

pulseStep :: (Circuit,[((String,String),Bool)]) -> (Circuit,[((String,String),Bool)])
pulseStep (c,((p,s),b):ps) =
  case c M.!? s of
    Nothing -> (c,ps)
    Just (Broadcast ts) -> (c,ps ++ map (\t -> ((s,t),b)) ts)
    Just (FlipFlop q ts) -> if b then (c,ps) else (M.insert s (FlipFlop (not q) ts) c,ps ++ map (\t -> ((s,t),not q)) ts)
    Just (Conjunction m ts) -> let m' = M.insert p b m; c' = M.insert s (Conjunction m' ts) c in (c',ps ++ map (\t -> ((s,t),not (and (M.elems m')))) ts)

broadcaster :: Parsec String () (String,Module)
broadcaster = do
  string "broadcaster -> "
  ts <- many1 letter `sepBy` string ", "
  spaces
  return ("broadcaster",Broadcast ts)

flipflop :: Parsec String () (String,Module)
flipflop = do
  string "%"
  name <- many1 letter
  string " -> "
  ts <- many1 letter `sepBy` string ", "
  spaces
  return (name,FlipFlop False ts)

conjunction :: Parsec String () (String,Module)
conjunction = do
  string "&"
  name <- many1 letter
  string " -> "
  ts <- many1 letter `sepBy` string ", "
  spaces
  return (name,Conjunction M.empty ts)

circuit :: Parsec String () Circuit
circuit = do
  ms <- many1 $ try broadcaster <|> try flipflop <|> conjunction
  return $ link $ M.fromList ms

predecs :: Circuit -> String -> [String]
predecs c s = M.foldrWithKey (\k m l -> if s `elem` targets m then k:l else l) [] c

link :: Circuit -> Circuit
link c = M.mapWithKey (\k m -> case m of (Conjunction _ ts) -> Conjunction (M.fromList $ map (,False) $ predecs c k) ts; m -> m) c

pulseCount :: Circuit -> ((Integer,Integer),Circuit)
pulseCount c = pulse' (c,[(("button","broadcaster"),False)]) 0 0
  where
    pulse' (c,[]) l h = ((l,h),c)
    pulse' (c,ps@((_,b):_)) l h = pulse' (pulseStep (c,ps)) (if b then l else l+1) (if b then h+1 else h)

pulseReport :: Circuit -> (Bool,Circuit)
pulseReport c = pulse' (c,[(("button","broadcaster"),False)]) False
  where
    pulse' (c,[]) l = (l,c)
    pulse' (c,ps@(((_,s),b):_)) l = pulse' (pulseStep (c,ps)) (l || (s == "rx" && not b))

pulseTrace :: Circuit -> ([(String,Bool)],Circuit)
pulseTrace c = pulse' (c,[(("button","broadcaster"),False)]) []
  where
    pulse' (c,[]) l = (l,c)
    pulse' (c,ps@(((_,s),b):_)) l = pulse' (pulseStep (c,ps)) ((s,b):l)

fullPulseTrace :: Circuit -> [[(String,Bool)]]
fullPulseTrace = unfoldr (Just . pulseTrace)

printTrace :: [[(String,Bool)]] -> IO ()
printTrace = mapM_ (\l -> putStrLn $ intercalate " " $ map (\(s,b) -> if b then s else "-" ++ s) l)

graphCircuit :: Circuit -> IO ()
graphCircuit c = do
  putStrLn "digraph {"
  forM_ (M.toList c) $ \(s,m) -> do
    putStrLn $ "  " ++ s ++ " [label=\""++ (case m of Broadcast _ -> ""; FlipFlop _ _ -> "%"; Conjunction _ _ -> "&") ++ s ++ "\"]"
  forM_ (M.toList c) $ \(s,m) -> do
    forM_ (targets m) $ \t -> do
      putStrLn $ "  " ++ s ++ " -> " ++ t
  putStrLn "}"

p1 :: Circuit -> Integer
p1 = uncurry (*) . foldl1 (\(l,r) (l',r') -> (l+l',r+r')) . take 1000 . unfoldr (Just . pulseCount)

p2 :: Circuit -> Integer
p2 = fromIntegral . length . unfoldr (\c -> (\(b,c') -> if b then Nothing else Just ((),c')) $ pulseReport c)

main :: Bool -> IO ()
main b = runParse 20 b circuit p1 p2

testA :: String
testA = unlines 
  ["broadcaster -> a, b, c",
  "%a -> b",
  "%b -> c",
  "%c -> inv",
  "&inv -> a"]

testB :: String
testB = unlines
  ["broadcaster -> a",
  "%a -> inv, con",
  "&inv -> b",
  "%b -> con",
  "&con -> output"]

testC :: String
testC = unlines
  ["broadcaster -> a",
  "%a -> b",
  "%b -> rx"]