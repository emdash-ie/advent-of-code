{-# LANGUAGE BlockArguments #-}
module Main where

import Data.Char (isDigit)

main :: IO ()
main = interact \input -> let
  digits = filter isDigit <$> lines input
  numbers :: [Integer]
  numbers = fmap (\ds -> read (head ds : [last ds])) digits
  in show (sum numbers)
