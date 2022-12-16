module Main where

import Data.Array hiding (bounds)
import qualified Data.Array as Array
import Data.Char (digitToInt)

main :: IO ()
main = interact $ \input -> let
  rows :: [[Int]]
  rows = fmap (fmap digitToInt) (lines input)
  numRows :: Int
  numRows = length rows
  numColumns :: Int
  numColumns = length (head rows)
  heights :: Array Int (Array Int Int)
  heights = listArray (0, numRows - 1) (fmap (listArray (0, numColumns - 1)) rows)
  top :: Int -> Int -> Int
  top 0 _ = 0
  top i j = let
    (t', _, _, _) = values ! (i - 1, j)
    in if heights ! i ! j > heights ! (i - 1) ! j
      then t' + 1
      else 1
  right :: Int -> Int -> Int
  right _ j | j == numColumns - 1 = 0
  right i j = let
    (_, r', _, _) = values ! (i, j + 1)
    in if heights ! i ! j > heights ! i ! (j + 1)
      then r' + 1
      else 1
  bottom :: Int -> Int -> Int
  bottom i _ | i == numRows - 1 = 0
  bottom i j = let
    (_, _, b', _) = values ! (i + 1, j)
    in if heights ! i ! j > heights ! (i + 1) ! j
      then b' + 1
      else 1
  left :: Int -> Int -> Int
  left _ 0 = 0
  left i j = let
    (_, _, _, l') = values ! (i, j - 1)
    in if heights ! i ! j > heights ! i ! (j - 1)
      then l' + 1
      else 1
  bounds :: ((Int, Int), (Int, Int))
  bounds = ((0, 0), (numRows - 1, numColumns - 1))
  values :: Array (Int, Int) (Int, Int, Int, Int)
  values = listArray bounds [ (top i j, right i j, bottom i j, left i j)
                            | (i, j) <- Array.range bounds
                            ]
  in show values
