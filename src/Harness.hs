module Harness where

import Control.Monad
import Data.Either
import Data.List
import Text.Parsec
import Text.Parsec.String
import System.IO.Unsafe

run :: (Show a, Show b) => Int -> Bool -> (String -> a) -> (String -> b) -> IO ()
run idx b p1 p2 = do
  f <- readFile $ "inputs/in" ++ show idx ++ ".txt"
  if b then print $ p2 f else print $ p1 f

runIO :: Int -> Bool -> (String -> IO ()) -> (String -> IO ()) -> IO ()
runIO idx b p1 p2 = do
  f <- readFile $ "inputs/in" ++ show idx ++ ".txt"
  (if b then p2 else p1) f

runList :: (Show a, Show c, Read b) => Int -> Bool -> ([b] -> a) -> ([b] -> c) -> IO ()
runList idx b p1 p2 = run idx b (p1 . map read . lines) (p2 . map read . lines)

runParse :: (Show b, Show c) => Int -> Bool -> Parsec String () a -> (a -> b) -> (a -> c) -> IO ()
runParse idx b p p1 p2 = do
  f <- readFile $ "inputs/in" ++ show idx ++ ".txt"
  let pr = parse p "" f
  either print (if b then print . p2 else print . p1) pr

debugParse :: Int -> Bool -> Parsec String () a -> (a -> IO ()) -> (a -> IO ()) -> IO ()
debugParse idx b p p1 p2 = do
  f <- readFile $ "inputs/in" ++ show idx ++ ".txt"
  let pr = parse p "" f
  either print (if b then p2 else p1) pr

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

lastM :: (Monad m) => m Bool -> m a -> m (Maybe a)
lastM b m = whileM b m >>= (\l -> return $ if null l then Nothing else Just $ last l)

neighbourhoods3 :: a -> [[a]] -> [((a, a, a), (a, a, a), (a, a, a))]
neighbourhoods3 e cs =
  let z1 = map (\l -> zip3 (e : l) l (tail l ++ [e])) cs
      re = repeat (e, e, e)
   in join (zipWith3 zip3 (re : z1) z1 (tail z1 ++ [re]))

neighbourhoods3Sq :: a -> [[a]] -> [[((a, a, a), (a, a, a), (a, a, a))]]
neighbourhoods3Sq e cs =
  let z1 = map (\l -> zip3 (e : l) l (tail l ++ [e])) cs
      re = repeat (e, e, e)
   in zipWith3 zip3 (re : z1) z1 (tail z1 ++ [re])

neighbourhoods5 :: a -> [[a]] -> [((a, a, a, a, a), (a, a, a, a, a), (a, a, a, a, a), (a, a, a, a, a), (a, a, a, a, a))]
neighbourhoods5 e cs =
  let z1 = map (\l -> zip5 (e : e : l) (e : l) l (tail l ++ [e]) (tail (tail l) ++ [e, e])) cs
      re = repeat (e, e, e, e, e)
   in join (zipWith5 zip5 (re : re : z1) (re : z1) z1 (tail z1 ++ [re]) (tail (tail z1) ++ [re, re]))

debugPrint :: (Show a) => a -> b -> b
debugPrint a b = unsafePerformIO (print a >> return b)

debugOut :: IO () -> b -> b
debugOut a b = unsafePerformIO (a >> return b)