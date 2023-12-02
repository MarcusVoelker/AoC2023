module Day2.Main where

import Text.Parsec
import Text.Parsec.Token
import Harness

data Color = Red | Green | Blue deriving (Show,Eq)
type Draw = [(Int,Color)]
data Game = Game { idx :: Int, draws :: [Draw] } deriving Show

color :: Stream s m Char => ParsecT s u m Color
color = (string "red" >> return Red) <|> (string "green" >> return Green) <|> (string "blue" >> return Blue)

draw :: Stream s m Char => ParsecT s u m Draw
draw = flip sepBy1 (string ",") $ do
  string " "
  i <- read <$> many1 digit
  string " "
  c <- color
  return (i,c)

game :: Stream s m Char => ParsecT s u m Game
game = do
  string "Game "
  i <- read <$> many1 digit
  string ":"
  gs <- sepBy1 draw (string ";")
  return $ Game i gs

counts :: (Int,Color) -> Bool
counts (i,Red) = i <= 12
counts (i,Green) = i <= 13
counts (i,Blue) = i <= 14

p1 :: [Game] -> Int
p1 gs = sum $ [i | Game i d <- gs, all (all counts) d]

maxCol :: Int -> Color -> Draw -> Int
maxCol v c d = maximum $ v:[i | (i,k) <- d, c==k]

p2 :: [Game] -> Int
p2 gs = sum $ map (product . foldr (\d [r,g,b] -> [maxCol r Red d,maxCol g Green d,maxCol b Blue d]) [0,0,0] . draws) gs

main :: Bool -> IO ()
main b = runParse 2 b (sepBy1 game (string "\n")) p1 p2