{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char (isDigit)

main :: IO ()
main = interact \input -> let
  digits = fmap (foldr findDigits ((Nothing, Nothing), "")) (lines input)
  numbers :: [Integer]
  numbers = fmap (\(ds, r) -> case ds of
                     (Just f, Just l) -> read (f : [l])
                     _ -> error ("pattern match: ds = " <> show ds <> "; r = " <> show r)
                 ) digits
  in show (sum numbers)

type Acc = ((Maybe Char, Maybe Char), String) -- first digit, last digit, rest of string

findDigits :: Char -> Acc -> Acc
findDigits c (ds, rest) = let
  rest' = c : rest
  in if isDigit c
    then (addDigit c ds, rest')
    else case findNumberWord c rest of
      Just d -> (addDigit d ds, rest')
      Nothing -> (ds, rest')
  where
    addDigit :: Char -> (Maybe Char, Maybe Char) -> (Maybe Char, Maybe Char)
    addDigit d (_, Nothing) = (Just d, Just d)
    addDigit d (_, l) = (Just d, l)

    findNumberWord :: Char -> String -> Maybe Char
    findNumberWord 'o' ('n' : 'e' : _) = Just '1'
    findNumberWord 't' ('w' : 'o' : _) = Just '2'
    findNumberWord 't' ('h' : 'r' : 'e' : 'e' : _) = Just '3'
    findNumberWord 'f' ('o' : 'u' : 'r' : _) = Just '4'
    findNumberWord 'f' ('i' : 'v' : 'e' : _) = Just '5'
    findNumberWord 's' ('i' : 'x' : _) = Just '6'
    findNumberWord 's' ('e' : 'v' : 'e' : 'n' : _) = Just '7'
    findNumberWord 'e' ('i' : 'g' : 'h' : 't' : _) = Just '8'
    findNumberWord 'n' ('i' : 'n' : 'e' : _) = Just '9'
    findNumberWord _ _ = Nothing
