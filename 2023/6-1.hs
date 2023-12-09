{-# LANGUAGE BlockArguments #-}
module Main where

main :: IO ()
main = interact \input -> let
  [times, distances] = fmap (fmap read . drop 1 . words) (lines input)
  f t d = case quadratic (negate 1) t (negate d) of
    (n, m) -> 1 + (m - n)
  possibles = zipWith f times distances
  in show (product possibles)

quadratic :: Integer -> Integer -> Integer -> (Integer, Integer)
quadratic a b c = (1 + floor (min plus minus), ceiling (max plus minus) - 1)
  where
    plus = (fromInteger left + right) / bottom
    minus = (fromInteger left - right) / bottom
    right :: Double
    right = sqrt (fromInteger (b * b) - (4 * fromInteger a * fromInteger c))
    left = negate b
    bottom = fromInteger (2 * a)
