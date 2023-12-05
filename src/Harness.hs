module Harness where

import Control.Monad
import Data.Either
import Data.List
import Text.Parsec
import Text.Parsec.String

run :: (Show a) => Int -> Bool -> (String -> a) -> (String -> a) -> IO ()
run idx b p1 p2 = do
  f <- readFile $ "inputs/in" ++ show idx ++ ".txt"
  print ((if b then p2 else p1) f)

runIO :: Int -> Bool -> (String -> IO ()) -> (String -> IO ()) -> IO ()
runIO idx b p1 p2 = do
  f <- readFile $ "inputs/in" ++ show idx ++ ".txt"
  (if b then p2 else p1) f

runList :: (Show a, Read b) => Int -> Bool -> ([b] -> a) -> ([b] -> a) -> IO ()
runList idx b p1 p2 = run idx b (p1 . map read . lines) (p2 . map read . lines)

runParse :: (Show b) => Int -> Bool -> Parsec String () a -> (a -> b) -> (a -> b) -> IO ()
runParse idx b p p1 p2 = do
  f <- readFile $ "inputs/in" ++ show idx ++ ".txt"
  let pr = parse p "" f
  either print (print . if b then p2 else p1) pr

withRemaining :: Parser a -> Parser (a, String)
withRemaining p = (,) <$> p <*> getInput

parsecToReadsPrec :: Parser a -> Int -> ReadS a
parsecToReadsPrec parsecParser prec input =
  case parse (withRemaining parsecParser) "" input of
    Left _ -> []
    Right result -> [result]

whileM :: (Monad m) => m Bool -> m a -> m [a]
whileM b m = do
  v <- b
  if v
    then m >>= (\x -> (x :) <$> whileM b m)
    else return []

neighbourhoods3 :: a -> [[a]] -> [((a, a, a), (a, a, a), (a, a, a))]
neighbourhoods3 e cs =
  let z1 = map (\l -> zip3 (e : l) l (tail l ++ [e])) cs
      re = repeat (e, e, e)
   in join (zipWith3 zip3 (re : z1) z1 (tail z1 ++ [re]))

neighbourhoods5 :: a -> [[a]] -> [((a, a, a, a, a), (a, a, a, a, a), (a, a, a, a, a), (a, a, a, a, a), (a, a, a, a, a))]
neighbourhoods5 e cs =
  let z1 = map (\l -> zip5 (e : e : l) (e : l) l (tail l ++ [e]) (tail (tail l) ++ [e, e])) cs
      re = repeat (e, e, e, e, e)
   in join (zipWith5 zip5 (re : re : z1) (re : z1) z1 (tail z1 ++ [re]) (tail (tail z1) ++ [re, re]))
