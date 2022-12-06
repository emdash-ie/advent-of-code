{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where

import Data.List (foldl')
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = Text.interact $ \input -> let
  changePosition Position{horizontal = h, depth = d, aim} t = let
    [direction, number] = Text.words t
    n = read (Text.unpack number) :: Integer
    in case direction of
      "down" -> Position h d (aim + n)
      "up" -> Position h d (aim - n)
      "forward" -> Position (h + n) (d + (aim * n)) aim
      _ -> Position 0 0 0
  Position{horizontal, depth} = foldl' changePosition (Position 0 0 0) (Text.lines input)
  in Text.pack (show (horizontal * depth))

data Position = Position
  { horizontal :: Integer
  , depth :: Integer
  , aim :: Integer
  } deriving (Show)
