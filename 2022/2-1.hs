{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text hiding (head, words, lines)
import Data.Monoid (Sum(..))

fuck :: Text
fuck = "fuck"

main :: IO ()
main = interact $ \input -> let
  roundScore :: Shape -> Shape -> Integer
  roundScore x y = let
    winScore = case toOutcome x y of
      P2Wins -> 6
      Draw -> 3
      P1Wins -> 0
    shapeScore = case y of
      Rock -> 1
      Paper -> 2
      Scissors -> 3
    in shapeScore + winScore
  shapes = fmap ((\x -> (x !! 0, x !! 1)) . fmap (toShape . head) . words) (lines input)
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

data Shape
  = Rock
  | Paper
  | Scissors
  deriving (Eq)

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
