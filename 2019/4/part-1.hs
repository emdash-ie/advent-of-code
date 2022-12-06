module Main where

main :: IO ()
main = print $ length (filter twoAdjacent (filter neverDecrease (fmap digits input)))

input :: [Integer]
input = [307237..769058]

twoAdjacent :: [Integer] -> Bool
twoAdjacent = any (uncurry (==)) . neighbours

neverDecrease :: [Integer] -> Bool
neverDecrease = all (uncurry (<=)) . neighbours

neighbours :: [a] -> [(a, a)]
neighbours xs = zip xs (drop 1 xs)

digits :: Integer -> [Integer]
digits i = reverse $ go i
  where
    go 0 = []
    go n = (n `mod` 10) : go (div n 10)
