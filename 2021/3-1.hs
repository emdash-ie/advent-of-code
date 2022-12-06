{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Prelude hiding (flip)
import Data.List (transpose)

main :: IO ()
main = interact $ \input -> let
  columns = transpose (lines input)
  gammaBits = mostCommon <$> columns
  epsilonBits = flip <$> gammaBits
  in show (toDecimal gammaBits * toDecimal epsilonBits)

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
