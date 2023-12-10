{-# LANGUAGE BlockArguments #-}
module Main where

import Debug.Trace

main :: IO ()
main = interact \input -> let
  histories :: [[Integer]]
  histories = fmap (fmap read . words) (lines input)
  predictions = fmap predict histories
  in show (sum predictions)

predict :: [Integer] -> Integer
predict h | all (== 0) h = 0
predict h = let
  Just (differences, _) = foldr (\this acc -> case acc of
                 Nothing -> Just ([], this)
                 Just (ds, next) -> Just (next - this : ds, this)) Nothing h
  in head h - predict differences
