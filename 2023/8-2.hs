{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import Prelude hiding (Left, Right)

import Control.Applicative
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Debug.Trace

main :: IO ()
main = interact \input -> let
  (directionLine, _ : _ : networkParagraph) = break (== '\n') input
  directions = fmap parseDirection directionLine
  directionKeys = take (length directions) [0..]
  directionMap = Map.fromList (zip directionKeys directions)

  network = parseNetwork networkParagraph

  distanceKeys :: [(Node, Integer)]
  distanceKeys = liftA2 (,) (Map.keys network) directionKeys
  distances :: Map (Node, Integer) (Integer, Node)
  distances = Map.fromList (fmap (\k -> (k, uncurry distance k)) distanceKeys)
  distance :: Node -> Integer -> (Integer, Node)
  distance node _ | (last node == 'Z') = (0, node)
  distance node di = let
    next = case Map.lookup di directionMap of Just Left -> fst; Just Right -> snd
    di' = (di + 1) `mod` toInteger (length directions)
    Just node' = fmap next (Map.lookup node network)
    d = 1 + fst (fromJust (Map.lookup (node', di') distances))
    in (d, node')

  starts = filter ((== 'A') . last) (Map.keys network)

  steps :: [Node] -> Integer -> Integer
  steps nodes di = let
    ds = mapMaybe (\n -> fmap fst (Map.lookup (n, di) distances)) nodes
    d = minimum ds
    next = case Map.lookup di directionMap of Just Left -> fst; Just Right -> snd
    di' = (di + 1) `mod` toInteger (length directions)
    nodes' = mapMaybe (\node -> fmap next (Map.lookup node network)) nodes
    in if all (== head ds) ds
      then head ds
      else 1 + steps nodes' di'

  in show (steps starts 0)

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
