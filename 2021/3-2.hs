{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Prelude hiding (flip)
import Data.List (intercalate, transpose, partition)
import Debug.Trace (trace)

main :: IO ()
main = interact $ \input -> let
  index = makeIndex (lines input)
  rs = makeRows index
  (co2, oxygen) = oxygenAndCO2 index
  product = (toDecimal co2 * toDecimal oxygen)
  in unlines [ "co2: " <> show co2
             , "toDecimal co2: " <> show (toDecimal co2)
             , "oxygen: " <> show oxygen
             , "toDecimal oxygen: " <> show (toDecimal oxygen)
             , "product: " <> show product
             ]
data Index = Leaf | Empty | Node Bit Index Index
  deriving (Show, Eq)

makeIndex :: [String] -> Index
makeIndex rows = let
  columnString = unlines rows
  f [] (zs, os) = (zs, os)
  f ('0' : ns) (zs, os) = (ns : zs, os)
  f ('1' : ns) (zs, os) = (zs, ns : os)
  f _ t = t
  (zs, os) = foldr f ([], []) rows
  (nz, no) = (length zs, length os)
  bit = if nz > no then Zero else One
  in if null rows
    then Empty
    else if all null rows
      then Leaf
      else Node bit (makeIndex zs) (makeIndex os)

makeRows :: Index -> [String]
makeRows Empty = []
makeRows Leaf = [[]]
makeRows (Node b left right) = let
  f c i =  (c :) <$> makeRows i
  in f '0' left <> f '1' right

oxygenAndCO2 :: Index -> ([Bit], [Bit])
oxygenAndCO2 Empty = ([], [])
oxygenAndCO2 Leaf = ([], [])
oxygenAndCO2 (Node _ Empty right) = let
  (co2, oxygen) = oxygenAndCO2 right
  in (One : co2, One : oxygen)
oxygenAndCO2 (Node _ left Empty) = let
  (co2, oxygen) = oxygenAndCO2 left
  in (Zero : co2, Zero : oxygen)
oxygenAndCO2 n@(Node bit left right) = let
  (uncommonTree, commonTree)
    = case bit of
        Zero -> (right, left)
        One -> (left, right)
  (co2, _) = oxygenAndCO2 uncommonTree
  (_, oxygen) = oxygenAndCO2 commonTree
  in (flip bit : co2, bit : oxygen)

mostCommon :: String -> Bit
mostCommon s = if zeroes > ones then Zero else One
  where
    (zeroes, ones) = foldr f (0, 0) s
    f :: Char -> (Integer, Integer) -> (Integer, Integer)
    f c (zeroes, ones) = case c of
          '0' -> (zeroes + 1, ones)
          '1' -> (zeroes, ones + 1)
          _ -> (zeroes, ones)

data Bit = Zero | One
  deriving (Show, Eq)

flip :: Bit -> Bit
flip Zero = One
flip One = Zero

toDecimal :: [Bit] -> Integer
toDecimal bs = let
  ps = zip (iterate (*2) 1) (reverse bs)
  f :: (Integer, Bit) -> Integer -> Integer
  f (_, Zero) acc = acc
  f (n, One) acc = acc + n
  in foldr f 0 ps
