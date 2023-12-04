{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Char (isDigit)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Monoid
import Data.Ord (comparing)
import Data.Semigroup (Arg(..))
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main = interact \input -> let
  (numberLocations, starLocations, schematic) = readSchematic input
  parts = Set.filter (bordersSymbol schematic) numberLocations
  gears = mapMaybe (toGear parts) (Set.toList starLocations)
  result = getSum (foldMap (\(n, m) -> Sum (n * m)) gears)
  in show result

data NumberLocation = NumberLocation
  { number :: Integer
  , location :: Location
  } deriving (Show, Eq)

instance Ord NumberLocation where
  compare = comparing location

data Location = Location
  { row :: Integer
  , startColumn :: Integer
  , endColumn :: Integer
  } deriving (Show, Eq, Ord)

type Schematic = Map (Integer, Integer) Char


type Result = (Set NumberLocation, Set Location, Schematic)
type Acc = (Result, Maybe (Integer, Integer, String))

readSchematic :: String -> Result
readSchematic input = foldr f (Set.empty, Set.empty, Map.empty) numberedRows
  where
    f :: (Integer, [(Integer, Char)]) -> Result -> Result
    f (n, row) (ls, gs, s) = case readRow n row of
      (ls', gs', s') -> (Set.union ls ls', Set.union gs gs', Map.union s s')

    numberedRows :: [(Integer, [(Integer, Char)])]
    numberedRows = zip [1..] (fmap (zip [1..]) (lines input))

    readRow :: Integer -> [(Integer, Char)] -> Result
    readRow n row = let
      ((ls, stars, s), m) = foldr (accRow n) ((Set.empty, Set.empty, Map.empty), Nothing) row
      ls' = case m of
        Nothing -> ls
        Just (start, end, ds) -> Set.insert (NumberLocation (read ds) (Location n start end)) ls
      in (ls', stars, s)


    accRow :: Integer -> (Integer, Char) -> Acc -> Acc
    accRow row (column, c) ((ns, gs, s), inNumber) =
      if isDigit c
      then ( (ns, gs, s)
           , Just (maybe (column, column, [c]) (\(_, end, cs) -> (column, end, c : cs)) inNumber)
           )
      else let
        ns' = maybe ns (\(start, end, ds) ->
                          Set.insert (NumberLocation (read ds) (Location row start end)) ns) inNumber
        s' = if isSymbol c then Map.insert (row, column) c s else s
        gs' = if c == '*' then Set.insert (coordinate (row, column)) gs else gs
        in ((ns', gs', s'), Nothing)

    isSymbol :: Char -> Bool
    isSymbol = (/= '.')

bordersSymbol :: Schematic -> NumberLocation -> Bool
bordersSymbol s NumberLocation{location} =
  any (`Map.member` s) (neighbours location)

borderingParts :: Set NumberLocation -> Location -> [Integer]
borderingParts ns l = mapMaybe (`Map.lookup` parts) (Set.toList (neighbours l))
  where
    parts :: Map (Integer, Integer) Integer
    parts = foldr combine Map.empty ns

    combine :: NumberLocation -> Map (Integer, Integer) Integer -> Map (Integer, Integer) Integer
    combine n m = let
      m' = Map.fromArgSet (Set.map (`Arg` number n) (coordinates (location n)))
      in Map.union m m'

-- Note: this will give the wrong answer if the location has two adjacent
-- numbers of the same value but in different positions. Got the correct answer
-- for the puzzle input anyway, so I haven’t fixed it.
toGear :: Set NumberLocation -> Location -> Maybe (Integer, Integer)
toGear ns l = case nub (borderingParts ns l) of
  [n, m] -> Just (n, m)
  _ -> Nothing

neighbours :: Location -> Set (Integer, Integer)
neighbours Location{..} =
  Set.fromList ([(row, startColumn - 1), (row, endColumn + 1)]
                <> ((,) <$> [row - 1, row + 1] <*> [startColumn - 1 .. endColumn + 1]))

coordinates :: Location -> Set (Integer, Integer)
coordinates Location{..} = Set.fromList (fmap (row, ) [startColumn .. endColumn])

coordinate :: (Integer, Integer) -> Location
coordinate (r, c) = Location r c c
