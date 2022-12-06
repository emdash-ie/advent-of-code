module Main where
import Data.Monoid (Sum (Sum))
import GHC.Arr ((!))
import qualified GHC.Arr as Array

main :: IO ()
main = interact solve

solve :: String -> String
solve input = let
  lanternfish :: [LanternFish]
  lanternfish = read ('[':input <> "]")
  in show (foldMap (descendantsA 256) lanternfish <> Sum (fromIntegral (length lanternfish)))

descendants :: Limit -> LanternFish -> Sum Integer
descendants n d = let
  birthDates = takeWhile (>= 0) (iterate (subtract 7) (n - d - 1))
  in foldMap (\n' -> 1 + descendants n' 8) birthDates

descendantsA :: Limit -> LanternFish -> Sum Integer
descendantsA limit = go limit
  where
    go n d = foldMap (\n' -> 1 + (ds ! (n', 8))) (birthDates n d)
    ds = Array.listArray bounds [go i j | (i, j) <- Array.range bounds]
    bounds = ((0, 0), (limit, 8))
    birthDates n d = takeWhile (>= 0) (iterate (subtract 7) (n - d - 1))

type LanternFish = Integer
type Limit = Integer
