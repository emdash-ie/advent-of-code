{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import Prelude hiding (Left, Right)

import Data.Map (Map)
import Data.Map qualified as Map
import Debug.Trace

main :: IO ()
main = interact \input -> let
  (directionLine, _ : _ : networkParagraph) = break (== '\n') input
  directions = fmap parseDirection directionLine
  network = parseNetwork networkParagraph
  in show (steps (cycle directions) network "AAA")

steps :: [Direction] -> Network -> Node -> Integer
steps _ _ "ZZZ" = 0
steps (d : ds) n node = let
  Just (l, r) = Map.lookup node n
  next = case d of Left -> l; Right -> r
  in 1 + steps ds n next

data Direction = Left | Right
  deriving (Show, Eq)

parseDirection :: Char -> Direction
parseDirection 'L' = Left
parseDirection 'R' = Right

type Network = Map Node (Node, Node)
type Node = String

parseNetwork :: String -> Network
parseNetwork paragraph = Map.unions connections
  where
    connections = fmap parseConnection (lines paragraph)

    parseConnection :: String -> Network
    parseConnection line = let
      [key, _, _ : first, second] = words line
      in Map.singleton key (take 3 first, take 3 second)
