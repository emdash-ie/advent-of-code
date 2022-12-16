{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Char (digitToInt)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Data.Set (Set)

main :: IO ()
main = interact $ \input -> let
  rows :: [[Cell]] -- rowIndex, columnIndex, value
  rows = zipWith (\n line ->
                    zip3 (repeat n) [1 ..] (fmap digitToInt line))
           [1 ..]
           (lines input)
  (left, right, top, bottom, visible) =
    foldl' processRow ([], [], Map.empty, Map.empty, Set.empty) rows
  processRow :: Acc -> [Cell] -> Acc
  processRow (lastLeft, lastRight, fromTop, fromBottom, oldVisible) = let
    newVisible =
      oldVisible `Set.union` Set.fromList lastLeft
      `Set.union` Set.fromList lastRight
    in foldl' processCell ([], [], fromTop, fromBottom, newVisible)
  processCell :: Acc -> Cell -> Acc
  processCell (fromLeft, fromRight, fromTop, fromBottom, v) c@(_, column, n) =
    let newFromLeft =
          if Just n > listToMaybe (fmap trd fromLeft)
          then
            c : fromLeft
          else fromLeft
        newFromRight =
          c : dropWhile ((<= n) . trd) fromRight
        newFromTop = Map.alter
          (\case
              Nothing ->
                Just (NonEmpty.singleton c)
              Just (x :| xs) ->
                if n > trd x
                then
                  Just (c :| (x : xs))
                else Just (x :| xs)
          )
          column
          fromTop
        newFromBottom = Map.alter
          (\case
              Nothing ->
                Just (NonEmpty.singleton c)
              Just (x :| xs) ->
                Just (c :| dropWhile ((<= n) . trd) (x : xs))
          )
          column
          fromBottom
     in (newFromLeft, newFromRight, newFromTop, newFromBottom, v)
  finalVisible =
    visible
    `Set.union` Set.fromList left
    `Set.union` Set.fromList right
    `Set.union` Set.unions (fmap (Set.fromList . NonEmpty.toList) (Map.elems top))
    `Set.union` Set.unions (fmap (Set.fromList . NonEmpty.toList) (Map.elems bottom))
  in show (Set.size finalVisible)

type Cell = (Int, Int, Int)
type Acc =
  -- fromLeft, fromRight, fromTop, fromBottom, visible
  ([Cell], [Cell], Map Int (NonEmpty Cell), Map Int (NonEmpty Cell), Set Cell)

trd :: Cell -> Int
trd (_, _, n) = n
