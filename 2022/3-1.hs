{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid (Sum(..))
import qualified Data.Set as Set
import Data.Char (isAsciiUpper, isAsciiLower)
import Data.Set (Set)
-- import Debug.Trace (trace)

main :: IO ()
main = interact $ \input -> let
  ls = lines input
  compartments :: [Set Char]
  compartments = fmap Set.fromList ls
  groups = go compartments
    where
      go :: [a] -> [(a, a, a)]
      go (x : y : z : rest) = (x, y, z) : go rest
      go _ = []
  items :: [Char]
  items = fmap (\(g1, g2, g3) ->
                  -- trace ("c1 = " <> show c1) $
                  -- trace ("c2 = " <> show c2) $
                  head (Set.toList (g1 `Set.intersection` (g2 `Set.intersection` g3))))
          groups
  priority :: Char -> Int
  priority c
    | isAsciiLower c
    = fromEnum c - fromEnum 'a' + 1
    | isAsciiUpper c
    = fromEnum c - fromEnum 'A' + 1 + 26
    | otherwise = error "fuck"
  prioritySum = foldMap (Sum . priority) items
  in show prioritySum

toShape :: Char -> Shape
toShape = \case
  'A' -> Rock
  'B' -> Paper
  'C' -> Scissors
  'X' -> Rock
  'Y' -> Paper
  'Z' -> Scissors
  c -> error ("unknown shape " <> [c])

toYourShape :: Shape -> Outcome -> Shape
toYourShape s Draw = s
toYourShape s P1Wins = losesTo s
toYourShape s P2Wins = beats s

losesTo :: Shape -> Shape
losesTo s = toEnum ((fromEnum s - 1) `mod` 3)

beats :: Shape -> Shape
beats s = toEnum ((fromEnum s + 1) `mod` 3)

data Shape
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Ord, Enum)

toOutcome :: Shape -> Shape -> Outcome
toOutcome c1 c2 | c1 == c2 = Draw
toOutcome Rock Scissors = P1Wins
toOutcome Scissors Paper = P1Wins
toOutcome Paper Rock = P1Wins
toOutcome _ _ = P2Wins

toDesiredOutcome :: Char -> Outcome
toDesiredOutcome = \case
  'X' -> P1Wins
  'Y' -> Draw
  'Z' -> P2Wins
  o -> error ("unknown desired outcome " <> [o])

data Outcome
  = P1Wins
  | P2Wins
  | Draw
