{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude as Prelude hiding (id)
import Data.List.NonEmpty hiding (filter)
import Data.Map.Strict as Map hiding (filter)
import Data.Maybe
import Data.Monoid
import Data.Semigroup
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Text.Read

main :: IO ()
main = Text.interact \input -> let
  games = fmap readGame (Text.lines input)
  totals = fmap totalReveals games
  powers = fmap power totals
  power rs = fromMaybe (error ("Missing colour in: " <> show rs)) do
    r <- Map.lookup Red rs
    g <- Map.lookup Green rs
    b <- Map.lookup Blue rs
    return (r * g * b)
  total = sum powers
  in Text.pack (show total)
  where
    readGame :: Text -> Game
    readGame line = let
      [gameText, revealsText] = Text.splitOn ": " line
      allRevealTexts = Text.splitOn "; " revealsText
      reveals = fromJust (nonEmpty (fmap readReveals allRevealTexts))
      in Game (readId gameText) reveals

    readId :: Text -> Integer
    readId t = fromMaybe (error ("Unrecognised game ID: " <> Text.unpack t)) do
      n <- Text.stripPrefix "Game " t
      readMaybe (Text.unpack n)

    readReveals :: Text -> Map Colour Integer
    readReveals t = let
      rs = Text.splitOn ", " t
      in Map.fromList (fmap readReveal rs)

    readReveal :: Text -> (Colour, Integer)
    readReveal t = let
      [n, c] = Text.split (== ' ') t
      in (readColour c, read (Text.unpack n))

    readColour :: Text -> Colour
    readColour "red" = Red
    readColour "blue" = Blue
    readColour "green" = Green
    readColour c = error ("Unrecognised colour: " <> Text.unpack c)

totalReveals :: Game -> Map Colour (Max Integer)
totalReveals Game{reveals} = Prelude.foldr (Map.unionWith (<>)) Map.empty (fmap (fmap Max) reveals)

data Game = Game
  { id :: Integer
  , reveals :: NonEmpty (Map Colour Integer)
  } deriving (Show, Eq)


data Colour
  = Red
  | Blue
  | Green
  deriving (Show, Eq, Ord)
