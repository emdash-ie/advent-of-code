{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Bifunctor (second)
import Data.Function
import Data.List
import Data.Ord

main :: IO ()
main = interact \input -> let
  readLine :: String -> ([Hand], Integer)
  readLine line = let
    [w1, w2] = words line
    in (readHand w1, read w2)
  readHand :: String -> [Hand]
  readHand s = let
    rawCards = take 5 (fmap parseCard s)
    wildHands :: HandCard -> [Hand] -> [Hand]
    wildHands (Normal c) [] = fmap (: []) $
      if c == Jack
      then fmap Wild [Jack .. Ace]
      else [Normal c]
    wildHands (Normal c) hands = do
      hand <- hands
      wildCard <- if c == Jack
        then fmap Wild [Jack .. Ace]
        else [Normal c]
      return (wildCard : hand)
    in foldr wildHands [] rawCards
  handBids :: [([Hand], Integer)]
  handBids = fmap readLine (lines input)
  rankedHandBids :: [(Rank, Hand, Integer)]
  rankedHandBids = fmap (\(hs, b) -> let
                            (r, h) = maximumBy (comparing (second (fmap asNormal)))
                                     (fmap (\h -> (classifyHand h, h)) hs)
                            in (r, h, b)) handBids
  result = sum (zipWith (\(_, _, b) n -> b * n)
                (sortOn (\(r, h, b) -> (r, fmap asNormal h, b)) rankedHandBids) [1..])
  in show result

type Hand = [HandCard]

data HandCard
  = Normal Card
  | Wild Card

asWild :: HandCard -> Card
asWild = \case
  Normal c -> c
  Wild c -> c

asNormal :: HandCard -> Card
asNormal = \case
  Normal c -> c
  Wild _ -> Jack

classifyHand :: Hand -> Rank
classifyHand h = case fmap length (groupBy ((==) `on` asWild) (sortOn asWild h)) of
  [_] -> FiveOfAKind
  [1, _] -> FourOfAKind
  [4, _] -> FourOfAKind
  [2, _] -> FullHouse
  [3, _] -> FullHouse
  [1, 1, _] -> ThreeOfAKind
  [1, 3, _] -> ThreeOfAKind
  [3, 1, _] -> ThreeOfAKind
  [2, 2, _] -> TwoPair
  [1, 2, _] -> TwoPair
  [2, 1, _] -> TwoPair
  [_, _, _, _] -> OnePair
  [_, _, _, _, _] -> HighCard

data Rank
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Show, Eq, Ord)

data Card
  = Jack
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Queen
  | King
  | Ace
  deriving (Show, Eq, Ord, Enum)

parseCard :: Char -> HandCard
parseCard = Normal . \case
  '2' -> Two
  '3' -> Three
  '4' -> Four
  '5' -> Five
  '6' -> Six
  '7' -> Seven
  '8' -> Eight
  '9' -> Nine
  'T' -> Ten
  'J' -> Jack
  'Q' -> Queen
  'K' -> King
  'A' -> Ace
