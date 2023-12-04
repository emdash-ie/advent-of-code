{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Applicative
import Data.Char (isDigit)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid

main :: IO ()
main = interact \input -> let
  (numberLocations, schematic) = readSchematic input
  parts = filter (bordersSymbol schematic) numberLocations
  result = getSum (foldMap (\NumberLocation{number} -> Sum number) parts)
  in show result

data NumberLocation = NumberLocation
  { number :: Integer
  , location :: Location
  } deriving Show

data Location = Location
  { row :: Integer
  , startColumn :: Integer
  , endColumn :: Integer
  } deriving (Show)

type Schematic = Map (Integer, Integer) Char


type Result = ([NumberLocation], Schematic)
type Acc = (Result, Maybe (Integer, Integer, String))

readSchematic :: String -> Result
readSchematic input = foldr f ([], Map.empty) numberedRows
  where
    f :: (Integer, [(Integer, Char)]) -> Result -> Result
    f (n, row) (ls, s) = case readRow n row of
      (ls', s') -> (ls <> ls', Map.union s s')

    numberedRows :: [(Integer, [(Integer, Char)])]
    numberedRows = zip [1..] (fmap (zip [1..]) (lines input))

    readRow :: Integer -> [(Integer, Char)] -> Result
    readRow n row = let
      ((ls, s), m) = foldr (accRow n) (([], Map.empty), Nothing) row
      ls' = case m of
        Nothing -> ls
        Just (start, end, ds) -> NumberLocation (read ds) (Location n start end) : ls
      in (ls', s)


    accRow :: Integer -> (Integer, Char) -> Acc -> Acc
    accRow row (column, c) ((ns, s), inNumber) =
      if isDigit c
      then ( (ns, s)
           , Just (maybe (column, column, [c]) (\(_, end, cs) -> (column, end, c : cs)) inNumber)
           )
      else let
        ns' = maybe ns (\(start, end, ds) ->
                          NumberLocation (read ds) (Location row start end) : ns) inNumber
        s' = if isSymbol c then Map.insert (row, column) c s else s
        in ((ns', s'), Nothing)

    isSymbol :: Char -> Bool
    isSymbol = (/= '.')

bordersSymbol :: Schematic -> NumberLocation -> Bool
bordersSymbol s NumberLocation{location = Location{row, startColumn, endColumn}} =
  any (`Map.member` s) possibleIndices
  where
    possibleIndices = liftA2 (,) [row - 1, row, row + 1] [startColumn - 1 .. endColumn + 1]
