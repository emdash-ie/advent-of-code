{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid (Sum(..))

main :: IO ()
main = interact $ \input -> let
  roundScore :: Shape -> Outcome -> Integer
  roundScore x y = let
    yourShape = toYourShape x y
    winScore = case y of
      P2Wins -> 6
      Draw -> 3
      P1Wins -> 0
    shapeScore = case yourShape of
      Rock -> 1
      Paper -> 2
      Scissors -> 3
    in shapeScore + winScore
  shapes = fmap ((\x -> (toShape (x !! 0), toDesiredOutcome (x !! 1))) . fmap head . words) (lines input)
  totalScore = foldMap (Sum . uncurry roundScore) shapes
  in show totalScore

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
