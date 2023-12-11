{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import Data.Map.Strict qualified as Map
import Data.List (transpose)

main :: IO ()
main = interact \input -> let
  image = lines input
  expand x i = if all (== '.') x
    then replicate 1000000 x <> i
    else x : i
  expandedImage = transpose (foldr expand [] (transpose (foldr expand [] image)))
  indexed :: [(Integer, [(Integer, Char)])]
  indexed = zip [1..] (fmap (zip [1..]) expandedImage)
  g _ (_, '.') acc = acc
  g rowIndex (columnIndex, '#') (gs, ds) = let
    ds' = foldr (\(r, c) -> Map.insert ((rowIndex, columnIndex), (r, c)) (abs (rowIndex - r) + abs (columnIndex - c))) ds gs
    in ((rowIndex, columnIndex) : gs, ds')
  f (rowIndex, row) acc = foldr (g rowIndex) acc row
  (galaxies, distances) = foldr f ([], Map.empty) indexed
  result = sum (Map.elems distances)
  in show result

type Image = [String]
