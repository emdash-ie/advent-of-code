{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Prelude hiding (lookup)
import Data.Foldable (foldl')
import Data.List (groupBy)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

main :: IO ()
main = interact \input -> let
  (seeds, maps) = parseInput input
  locations = fmap (\s -> foldl' lookup s maps) seeds
  result = minimum locations
  in show ("Number of seeds: " <> show (length seeds))

type Seed = Integer
type Input = ([Seed], [RangeMap])

type Source = Integer
type Destination = Integer
type Length = Integer

newtype RangeMap = RangeMap
  { mappings :: [(Destination, Source, Length)]
  } deriving (Show)

lookup :: Source -> RangeMap -> Source
lookup source RangeMap{mappings} =
  trace ("Looking up " <> show source) $
  fromMaybe source (foldr (\(d, s, l) -> \case
                                                               Nothing ->
                                                                 if s <= source && source < s + l
                                                                 then Just (d + (source - s))
                                                                 else Nothing
                                                               x -> x)
                                                      Nothing
                                                      mappings)

parseInput :: String -> Input
parseInput input = (seeds, maps)
  where
    maps = fmap (readMap . takeWhile (not . null) . drop 2) paragraphs
    seeds = do
      (start, length) <- pair seedRanges
      take (fromIntegral length) [start ..]

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
