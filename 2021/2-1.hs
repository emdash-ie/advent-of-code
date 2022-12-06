{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = Text.interact $ \input -> let
  makePosition t = let
    [d, number] = Text.words t
    n = read (Text.unpack number) :: Integer
    in case d of
      "forward" -> Position n 0
      "up" -> Position 0 (-n)
      "down" -> Position 0 n
      _ -> Position 0 0
  Position{horizontal, vertical} = foldMap makePosition (Text.lines input)
  in Text.pack (show (horizontal * vertical))

data Position = Position
  { horizontal :: Integer
  , vertical :: Integer
  } deriving (Show)

instance Semigroup Position where
  Position h1 v1 <> Position h2 v2 = Position (h1 + h2) (v1 + v2)

instance Monoid Position where
  mempty = Position 0 0
  mappend = (<>)
