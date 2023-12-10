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
  Just (differences, _, n) = foldr (\this acc -> case acc of
                 Nothing -> Just ([], this, this)
                 Just (ds, previous, last) -> Just (previous - this : ds, this, last)) Nothing h
  in n + predict differences
