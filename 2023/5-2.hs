{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Prelude hiding (lookup)
import Data.Foldable (foldl')
import Data.List (groupBy, sortOn)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

main :: IO ()
main = interact \input -> let
  (seeds, unsortedMaps) = parseInput input
  maps = fmap sortMap unsortedMaps
  sortMap RangeMap{mappings} = RangeMap (sortOn (\(_, s, _) -> s) mappings)
  locations = foldl' (\seedRanges map -> seedRanges >>= (`lookupRange` map)) seeds maps
  result = minimum locations
  in show (result)

type SeedRange = (Source, Length)
type Input = ([SeedRange], [RangeMap])

type Source = Integer
type Destination = Integer
type Length = Integer

newtype RangeMap = RangeMap
  { mappings :: [(Destination, Source, Length)]
  } deriving (Show)

lookupRange :: (Source, Length) -> RangeMap -> [(Source, Length)]
lookupRange (start, length) RangeMap{mappings} = case mappings of
  [] -> [(start, length)]
  (dest, source, length') : ms -> let
    end = start + length - 1
    beforeMapping = if start < source
      then [(start, source - start)]
      else []
    afterMapping = if end >= source + length'
      then let
        start' = max start (source + length')
        in lookupRange (start', end - start' + 1) (RangeMap ms)
      else []
    mapped = if end < source || start >= source + length'
      then []
      else let
        start' = max source start
        end' = min end (source + length' - 1)
        in [(start' - source + dest, end' - start' + 1)]
    in beforeMapping <> mapped <> afterMapping

parseInput :: String -> Input
parseInput input = (seeds, maps)
  where
    maps = fmap (readMap . takeWhile (not . null) . drop 2) paragraphs
    seeds = pair seedRanges

    seedRanges = fmap read (drop 1 (words seedsLine))
    [seedsLine] : paragraphs = groupBy (const (not . null)) (lines input)

readMap :: [String] -> RangeMap
readMap = foldr f RangeMap{mappings = []}
  where
    f :: String -> RangeMap -> RangeMap
    f line RangeMap{mappings} = let
      [destStart, sourceStart, length] = fmap read (words line)
      in RangeMap{mappings = (destStart, sourceStart, length) : mappings}

pair :: [a] -> [(a, a)]
pair xs = case splitAt 2 xs of
  ([x, y], rest) -> (x, y) : pair rest
  _ -> []
