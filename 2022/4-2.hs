module Main where

import Data.Maybe (mapMaybe)
-- import Debug.Trace (trace)

main :: IO ()
main = interact $ \input -> let
  ls = lines input
  toRange :: String -> (Integer, Integer)
  toRange s = let
    (n, '-' : m) = break (== '-') s
    in (read n, read m)
  pairs :: [((Integer, Integer), (Integer, Integer))]
  pairs = do
    l <- ls
    let (r1, ',' : r2) = break (== ',') l
    return (toRange r1, toRange r2)
  overlaps :: ((Integer, Integer), (Integer, Integer)) -> Maybe ()
  overlaps ((a, b), (c, d))
    | a >= c && a <= d = Just ()
    | b >= c && b <= d = Just ()
    | c >= a && c <= b = Just ()
    | d >= a && d <= b = Just ()
    | otherwise = Nothing
  in show (length (mapMaybe overlaps pairs))
