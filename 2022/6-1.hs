module Main where
import Data.List (zip5, nub)

main :: IO ()
main = interact $ \input -> let
  stream = zip5 input (drop 1 input) (drop 2 input) (drop 3 input) [4 :: Integer ..]
  (_, _, _, _, n):_ = dropWhile (\(a, b, c, d, _) -> length (nub [a, b, c, d]) < 4) stream
  in show n
