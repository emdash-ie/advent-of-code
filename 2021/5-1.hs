{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Char (isSpace)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Monoid (Sum)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read.Lex (numberToRangedRational)

main :: IO ()
main = interact solve

solve :: String -> String
solve input = let
  ls = readLine <$> lines input
  points = Map.unionsWith (<>) (coveredPoints <$> ls)
  swap (x, y) = (y, x)
  inverted = Map.fromListWith Set.union (fmap (fmap Set.singleton . swap) (Map.toList points))
  in show (sum (fmap snd (Map.toList (fmap Set.size (Map.withoutKeys inverted (Set.singleton 1))))))

coveredPoints :: Line -> Map Point (Sum Integer)
coveredPoints Line {start = s , end = e} = Map.fromList (fmap (, 1) (go s e))
  where
    go :: Point -> Point -> [Point]
    go start end
      | start == end = [start]
      | otherwise = let
          diffX = signum (x end - x start)
          diffY = signum (y end - y start)
          in start : go (Point (x start + diffX) (y start + diffY)) end

isHorizontalOrVertical :: Line -> Bool
isHorizontalOrVertical Line
  { start = Point{x = startX, y = startY}
  , end = Point{x = endX, y = endY}
  } = startX == endX || startY == endY

readLine :: String -> Line
readLine s = let
  (p1, ' ':'-':'>':' ' : p2) = break isSpace s
  start = readPoint p1
  end = readPoint p2
  in Line { start, end }

readPoint :: String -> Point
readPoint s = let
  (n1, ',' : n2) = break (== ',') s
  in Point { x = read n1, y = read n2 }

data Line = Line { start :: Point, end :: Point } deriving (Show, Eq)

data Point = Point { x :: Integer, y :: Integer } deriving (Show, Eq, Ord)
