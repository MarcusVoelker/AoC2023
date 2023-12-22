module Day19.Main where

import Harness
import Text.Parsec hiding ((<|>))
import Control.Applicative
import Data.Maybe
import Data.List
import qualified Data.Map as M

data Rule = Less { p :: Char, b :: Int, t :: String } | More { p :: Char, b :: Int, t :: String }
data Workflow = Workflow { name :: String, rules :: [Rule], def :: String}
data Part = Part { x :: Int, m :: Int, a :: Int, s :: Int}
data IPart = IPart { xI :: (Int,Int), mI :: (Int,Int), aI :: (Int,Int), sI :: (Int,Int)}

parser :: Parsec String () (M.Map String Workflow, [Part])
parser = do
  ws <- many1 $ do
    idt <- many1 letter
    string "{"
    rs <- many1 $ do
      lookAhead $ try $ do
        oneOf "xmas"
        oneOf "<>"
        return ()
      p <- oneOf "xmas"
      c <- oneOf "<>"
      b <- read <$> many1 digit
      string ":"
      t <- many1 letter
      string ","
      if c == '<' then return $ Less p b t else return $ More p b t
    d <- many1 letter
    string "}\n"
    return $ Workflow { name = idt, rules = rs, def = d }
  spaces
  rs <- many1 $ do
    string "{x="
    x <- read <$> many1 digit
    string ",m="
    m <- read <$> many1 digit
    string ",a="
    a <- read <$> many1 digit
    string ",s="
    s <- read <$> many1 digit
    string "}\n"
    return $ Part { x = x, m = m, a = a, s = s }
  return (M.fromList $ map (\w -> (name w, w)) ws, rs)

tryRule :: Rule -> Part -> Maybe String
tryRule (Less 'x' b t) (Part x _ _ _) | x < b = Just t
tryRule (Less 'm' b t) (Part _ m _ _) | m < b = Just t
tryRule (Less 'a' b t) (Part _ _ a _) | a < b = Just t
tryRule (Less 's' b t) (Part _ _ _ s) | s < b = Just t
tryRule (More 'x' b t) (Part x _ _ _) | x > b = Just t
tryRule (More 'm' b t) (Part _ m _ _) | m > b = Just t
tryRule (More 'a' b t) (Part _ _ a _) | a > b = Just t
tryRule (More 's' b t) (Part _ _ _ s) | s > b = Just t
tryRule _ _ = Nothing

interpretStep :: M.Map String Workflow -> Part -> String -> String
interpretStep ws p s = let
  w = ws M.! s
  in fromMaybe (def w) $ foldl1 (<|>) (map (`tryRule` p) $ rules w)

interpret :: M.Map String Workflow -> Part -> Bool
interpret ws p = interpret' ws p "in"
  where
    interpret' ws p "A" = True
    interpret' ws p "R" = False
    interpret' ws p s = interpret' ws p $ interpretStep ws p s

p1 :: (M.Map String Workflow, [Part]) -> Integer
p1 (ws,ps) = fromIntegral $ sum $ map (\p -> x p + m p + a p + s p) $ filter (interpret ws) ps

iRule :: Rule -> IPart -> (Maybe (String,IPart),Maybe IPart)
iRule (Less 'x' b t) p@(IPart (xl,xu) (ml,mu) (al,au) (sl,su))
  | xu < b = (Just (t,p),Nothing)
  | xl >= b = (Nothing,Just p)
  | otherwise = (Just (t,IPart (xl,b-1) (ml,mu) (al,au) (sl,su)),Just $ IPart (b,xu) (ml,mu) (al,au) (sl,su))
iRule (Less 'm' b t) p@(IPart (xl,xu) (ml,mu) (al,au) (sl,su))
  | mu < b = (Just (t,p),Nothing)
  | ml >= b = (Nothing,Just p)
  | otherwise = (Just (t,IPart (xl,xu) (ml,b-1) (al,au) (sl,su)),Just $ IPart (xl,xu) (b,mu) (al,au) (sl,su))
iRule (Less 'a' b t) p@(IPart (xl,xu) (ml,mu) (al,au) (sl,su))
  | au < b = (Just (t,p),Nothing)
  | al >= b = (Nothing,Just p)
  | otherwise = (Just (t,IPart (xl,xu) (ml,mu) (al,b-1) (sl,su)),Just $ IPart (xl,xu) (ml,mu) (b,au) (sl,su))
iRule (Less 's' b t) p@(IPart (xl,xu) (ml,mu) (al,au) (sl,su))
  | su < b = (Just (t,p),Nothing)
  | sl >= b = (Nothing,Just p)
  | otherwise = (Just (t,IPart (xl,xu) (ml,mu) (al,au) (sl,b-1)),Just $ IPart (xl,xu) (ml,mu) (al,au) (b,su))
iRule (More 'x' b t) p@(IPart (xl,xu) (ml,mu) (al,au) (sl,su))
  | xl > b = (Just (t,p),Nothing)
  | xu <= b = (Nothing,Just p)
  | otherwise = (Just (t,IPart (b+1,xu) (ml,mu) (al,au) (sl,su)),Just $ IPart (xl,b) (ml,mu) (al,au) (sl,su))
iRule (More 'm' b t) p@(IPart (xl,xu) (ml,mu) (al,au) (sl,su))
  | ml > b = (Just (t,p),Nothing)
  | mu <= b = (Nothing,Just p)
  | otherwise = (Just (t,IPart (xl,xu) (b+1,mu) (al,au) (sl,su)),Just $ IPart (xl,xu) (ml,b) (al,au) (sl,su))
iRule (More 'a' b t) p@(IPart (xl,xu) (ml,mu) (al,au) (sl,su))
  | al > b = (Just (t,p),Nothing)
  | au <= b = (Nothing,Just p)
  | otherwise = (Just (t,IPart (xl,xu) (ml,mu) (b+1,au) (sl,su)),Just $ IPart (xl,xu) (ml,mu) (al,b) (sl,su))
iRule (More 's' b t) p@(IPart (xl,xu) (ml,mu) (al,au) (sl,su))
  | sl > b = (Just (t,p),Nothing)
  | su <= b = (Nothing,Just p)
  | otherwise = (Just (t,IPart (xl,xu) (ml,mu) (al,au) (b+1,su)),Just $ IPart (xl,xu) (ml,mu) (al,au) (sl,b))

foldRuleI :: IPart -> String -> [Rule] -> [(String,IPart)]
foldRuleI i s [] = [(s,i)]
foldRuleI i s (r:rs) = case iRule r i of
    (Just res,Nothing) -> [res]
    (Nothing,Just cont) -> foldRuleI cont s rs
    (Just res,Just cont) -> res : foldRuleI cont s rs

interpretStepI :: M.Map String Workflow -> IPart -> String -> [(String,IPart)]
interpretStepI ws p s = let
  w = ws M.! s
  rs = rules w
  in foldRuleI p (def w) rs

interpretI :: M.Map String Workflow -> IPart -> [IPart]
interpretI ws p = interpret' p "in"
  where
    interpret' p "A" = [p]
    interpret' p "R" = []
    interpret' p s = interpretStepI ws p s >>= (\(s,p) -> interpret' p s)

p2 :: (M.Map String Workflow, [Part]) -> Integer
p2 (ws,_) = sum $ map (\(IPart (xl,xu) (ml,mu) (al,au) (sl,su)) -> (fromIntegral xu-fromIntegral xl+1)*(fromIntegral mu-fromIntegral ml+1)*(fromIntegral au-fromIntegral al+1)*(fromIntegral su-fromIntegral sl+1))$ interpretI ws (IPart (1,4000) (1,4000) (1,4000) (1,4000))

main :: Bool -> IO ()
main b = runParse 19 b parser p1 p2