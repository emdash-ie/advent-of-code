{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Array
import Data.Bool
import Data.Char (digitToInt)
import Data.Monoid

data ViewingDistance = ViewingDistance
  { top :: Bool
  , right :: Bool
  , bottom :: Bool
  , left :: Bool
  } deriving (Show)

type Height = Int

main :: IO ()
main = interact $ \input -> let

  rows :: [[Height]]
  rows = fmap (fmap digitToInt) (lines input)

  numRows :: Int
  numRows = length rows

  numColumns :: Int
  numColumns = length (rows !! 0)

  heights :: Array Int (Array Int Height)
  heights = listArray (0, numRows - 1) (fmap (listArray (0, numColumns - 1)) rows)

  viewingDistance :: (Int, Int) -> ViewingDistance
  viewingDistance (row, column) = let
    height = heights ! row ! column
    in ViewingDistance
      { top = or
        ((row == 0) :
         fmap (\r -> top (viewingDistances ! (r, column)))
             (takeWhile (\r -> let
                           h' = heights ! r ! column
                           in r >= 0 && height > h') (iterate (subtract 1) (row - 1))))
      , right = or
        ((column == numColumns - 1) :
         fmap (\c -> right (viewingDistances ! (row, c)))
            (takeWhile (\c -> (height > (heights ! row ! c)))
             [column + 1 .. numColumns - 1]))
      , bottom = or
        ((row == numRows - 1) :
         fmap (\r -> bottom (viewingDistances ! (r, column)))
            (takeWhile (\r -> (height > (heights ! r ! column)))
             [row + 1 .. numRows - 1]))
      , left = or
        ((column == 0) :
         fmap (\c -> left (viewingDistances ! (row, c)))
            (takeWhile (\c -> c >= 0 && height > (heights ! row ! c))
             (iterate (subtract 1) (column - 1))))
      }

  viewingDistances :: Array (Int, Int) ViewingDistance
  viewingDistances = listArray ((0, 0), (numRows - 1, numColumns - 1)) [ viewingDistance (row, column)
                                                                   | (row, column) <- range ((0, 0), (numRows - 1, numColumns - 1))
                                                                   ]

  _visibleCount :: Integer
  _visibleCount = getSum (foldMap (\ViewingDistance{..} ->
                              if or [top, right, bottom, left]
                              then 1
                              else 0)
                    viewingDistances)

  viewingDistanceCounts :: [[Integer]]
  viewingDistanceCounts = flip fmap [0 .. numRows - 1]
      (\row -> flip fmap [0 .. numColumns - 1] (\column -> f row column))
    where
    f row column =
      case viewingDistances ! (row, column) of
        ViewingDistance{..} ->
          sum (fmap (bool 0 1) [top, right, bottom, left])

  prettyViewingDistances :: String
  prettyViewingDistances = unlines (fmap (foldMap show) viewingDistanceCounts)

  in prettyViewingDistances
