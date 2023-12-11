{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import Data.Map.Strict qualified as Map
import Data.List (transpose)

main :: IO ()
main = interact \input -> let
  image = lines input
  expand i x = if all (`elem` ['.', ' ']) x
    then replicate (length x) ' ' : i
    else x : i
  expandedImage :: [String]
  expandedImage = transpose (foldr (flip expand) [] (transpose (foldr (flip expand) [] image)))
  indexColumns :: String -> [(Integer, Char)]
  indexColumns = snd . foldr (\x (n, xs) -> let
                           n' = case x of
                             ' ' -> n + 1000000
                             _ -> n + 1
                           in (n', (n, x) : xs)
                       ) (1, [])
  indexRows :: [[(Integer, Char)]] -> [(Integer, [(Integer, Char)])]
  indexRows = snd . foldr (\x (n, xs) -> let
                        n' = case x of
                          (_, ' ') : _ -> n + 1000000
                          _ -> n + 1
                        in (n', (n, x) : xs)
                    ) (1, [])
  indexed :: [(Integer, [(Integer, Char)])]
  indexed = indexRows (fmap indexColumns expandedImage)
  g _ acc (_, '.') = acc
  g _ acc (_, ' ') = acc
  g rowIndex (gs, ds) (columnIndex, '#') = let
    ds' = foldr (flip \m (r, c) ->
        Map.insert ((rowIndex, columnIndex), (r, c)) (abs (rowIndex - r) + abs (columnIndex - c)) m) ds gs
    in ((rowIndex, columnIndex) : gs, ds')
  f acc (rowIndex, row) = foldr (flip (g rowIndex)) acc row
  (galaxies, distances) = foldr (flip f) ([], Map.empty) indexed
  result = sum (Map.elems distances)
  picture = unlines expandedImage <> "\n\n" <> show galaxies <> "\n\n" <> show distances
  in show result

type Image = [String]
