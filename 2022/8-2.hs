module Main where

import Data.Char (digitToInt)
import Data.List (foldl', transpose)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)

main :: IO ()
main = interact $ \input -> let
  rows :: [[Cell]] -- rowIndex, columnIndex, value
  rows = zipWith (\n line ->
                    zip3 (repeat n) [1 ..] (fmap digitToInt line))
           [1 ..]
           (lines input)
  columns :: [[Cell]]
  columns = transpose rows
  leftVisible :: Map (Int, Int) Int
  leftVisible = Map.unions (fmap (scenicForLine Map.empty) rows)
  rightVisible :: Map (Int, Int) Int
  rightVisible = Map.unions (fmap (scenicForLine Map.empty . reverse) rows)
  topVisible :: Map (Int, Int) Int
  topVisible = Map.unions (fmap (scenicForLine Map.empty) columns)
  bottomVisible :: Map (Int, Int) Int
  bottomVisible = Map.unions (fmap (scenicForLine Map.empty . reverse) columns)
  allVisible :: Map (Int, Int) Int
  allVisible =
    Map.unionWith (*)
      leftVisible
      (Map.unionWith (*)
        rightVisible
        (Map.unionWith (*) bottomVisible topVisible))
  in show (maximum (Map.elems allVisible))

scenicForLine ::
  Map (Int, Int) Int ->
  [Cell] ->
  Map (Int, Int) Int
scenicForLine sm = snd . foldl' f (edgeMap, sm)
  where
    f ::
      (Map Int Int, Map (Int, Int) Int) ->
      Cell ->
      (Map Int Int, Map (Int, Int) Int)
    f (viewMap, scoreMap) (row, column, height) = let
      newScoreMap = Map.insert
        (row, column)
        (fromMaybe (error "no key") (Map.lookup height viewMap))
        scoreMap
      viewUpdates = [ if n <= height
                      then Map.insert n 1
                      else Map.adjust (+ 1) n
                    | n <- [0 .. 9]
                    ]
      in (foldr ($) viewMap viewUpdates, newScoreMap)

    edgeMap :: Map Int Int
    edgeMap = Map.fromList [(n, 0) | n <- [0 .. 9]]

type Cell = (Int, Int, Int)
