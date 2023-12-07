{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Functor
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

main :: IO ()
main = Text.interact \input -> let
  cards = fmap parseCard (Text.lines input)
  in Text.pack (show (sum (fmap points cards)))

data Card = Card
  { winningNumbers :: Set Integer
  , yourNumbers :: Set Integer
  } deriving (Show, Eq)

parseCard :: Text -> Card
parseCard line = let
  [prefix, rest] = Text.split (== ':') line
  [winningNumbers, yourNumbers] = Text.split (== '|') rest <&>
    (Set.fromList . fmap (read . Text.unpack) . filter (not . Text.null) . Text.split (== ' '))
  in Card{..}

points :: Card -> Integer
points Card{winningNumbers, yourNumbers} = let
  size = Set.size (Set.intersection winningNumbers yourNumbers)
  in if size == 0 then 0 else 2 ^ (size - 1)
