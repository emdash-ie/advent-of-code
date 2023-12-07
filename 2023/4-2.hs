{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Char (isDigit)
import Data.Functor
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Text.Read

main :: IO ()
main = Text.interact \input -> let
  cards = fmap parseCard (Text.lines input)
  winningsMap = foldr (flip winnings) Map.empty cards
  in Text.pack (show (sum winningsMap))

data Card = Card
  { number :: Integer
  , winningNumbers :: Set Integer
  , yourNumbers :: Set Integer
  } deriving (Show, Eq)

parseCard :: Text -> Card
parseCard line = let
  [prefix, rest] = Text.split (== ':') line
  number = fromMaybe (error ("Failed to read prefix: " <> Text.unpack prefix))
    (readMaybe (Text.unpack (Text.takeWhile isDigit (Text.dropWhile (not . isDigit) prefix))))
  [winningNumbers, yourNumbers] = Text.split (== '|') rest <&>
    (Set.fromList . fmap (read . Text.unpack) . filter (not . Text.null) . Text.split (== ' '))
  in Card{..}

winnings :: Map Integer Integer -> Card -> Map Integer Integer
winnings cards Card{number, winningNumbers, yourNumbers} = let
  size = toInteger (Set.size (Set.intersection winningNumbers yourNumbers))
  wonCardNumbers = [number + 1 .. number + size]
  wonCards = 1 + sum (fmap (fromMaybe (error "oh no!") . (`Map.lookup` cards)) wonCardNumbers)
  in Map.insert number wonCards cards
