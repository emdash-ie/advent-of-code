module Main where

import Data.List (group)

main :: IO ()
main = print $ length (filter test input)

input :: [Integer]
input = [307237..769058]

test :: Integer -> Bool
test n = let
  ds = digits n
  in onlyTwoAdjacent ds && neverDecrease ds

twoAdjacent :: [Integer] -> Bool
twoAdjacent = any (uncurry (==)) . neighbours

onlyTwoAdjacent :: [Integer] -> Bool
onlyTwoAdjacent ds = elem 2 (fmap length (group ds))

neverDecrease :: [Integer] -> Bool
neverDecrease = all (uncurry (<=)) . neighbours

neighbours :: [a] -> [(a, a)]
neighbours xs = zip xs (drop 1 xs)

digits :: Integer -> [Integer]
digits i = reverse $ go i
  where
    go 0 = []
    go n = (n `mod` 10) : go (div n 10)
