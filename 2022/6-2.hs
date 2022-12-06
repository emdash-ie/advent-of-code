module Main where
import Data.List (nub, transpose)

main :: IO ()
main = interact $ \input -> let
  stream = zip (transpose (fmap (`drop` input) [0 .. 13])) [14 :: Integer ..]
  (_, n):_ = dropWhile (\(xs, _) -> length (nub xs) < 14) stream
  in show n
