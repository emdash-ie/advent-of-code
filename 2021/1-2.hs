module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = Text.interact $ \input -> let
  numbers :: [Integer]
  numbers = read . Text.unpack <$> Text.lines input
  triples = zip3 numbers (drop 1 numbers) (drop 2 numbers)
  g :: (Integer, Integer, Integer) -> Integer
  g (x, y, z) = x + y + z
  pairsOfSums = zip (g <$> triples) (g <$> drop 1 triples)
  f :: (Integer, Integer) -> Integer -> Integer
  f (x, y) n = if x < y then n + 1 else n
  in Text.pack (show (foldr f 0 pairsOfSums))
