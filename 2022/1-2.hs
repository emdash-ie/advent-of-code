module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Read (readMaybe)
import Data.Bifunctor (bimap)
import Data.Maybe (isJust, catMaybes)
import Data.List (groupBy, sortBy)
import Data.Ord (Down (..), comparing)

main :: IO ()
main = Text.interact $ \input -> let
  calories :: [Maybe Integer]
  calories = readMaybe . Text.unpack <$> Text.lines input
  g :: Maybe Integer -> Maybe Integer -> Bool
  g =  curry (uncurry (&&) . bimap isJust isJust)
  caloriesPerElf :: [[Integer]]
  caloriesPerElf = fmap catMaybes (groupBy g calories)
  topThree :: [Integer]
  topThree = take 3 (sortBy (comparing Down) (fmap sum caloriesPerElf))
  in Text.pack (show (sum topThree))
