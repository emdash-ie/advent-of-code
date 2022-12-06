{-# LANGUAGE TupleSections #-}
module Main where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Debug.Trace (trace)
import Data.Monoid (Sum(..), First (First, getFirst))
import Data.Char (isDigit, isSpace)
import Data.Bool (bool)
import qualified Data.List as List
import Data.List (foldl')

main :: IO ()
main = interact $ \input -> let
  draw : "" : boards = lines input
  readDraw :: String -> [Integer]
  readDraw [] = []
  readDraw d = let
    (n, rest) = break (== ',') d
    in read n : readDraw (drop 1 rest)
  (bs, w) = foldl' checkForWinner (readBoards boards, Nothing) (readDraw draw)
  in case w of
    Nothing -> "No winner!"
    Just (winner, called) -> show (scoreBoard winner called)

checkForWinner :: ([Board], Maybe (Board, Integer)) -> Integer -> ([Board], Maybe (Board, Integer))
checkForWinner t@(_, Just _) called = t
checkForWinner (boards, Nothing) called = let
  bs = markBoards called boards
  w = foldMap (First . \b -> if boardIsWinner b then Just b else Nothing) bs
  in (bs, (, called) <$> getFirst w)

markBoards :: Integer -> [Board] -> [Board]
markBoards n = (fmap . fmap . fmap) (\e -> e {marked = marked e || (number e == n)})

boardIsWinner :: Board -> Bool
boardIsWinner b = any (all marked) b || any (all marked) (transpose b)

prettyBoard :: Board -> String
prettyBoard b = unlines (Vector.toList (show <$> b))

transpose :: Board -> Board
transpose =
  Vector.fromList . fmap Vector.fromList
  . List.transpose
  . fmap Vector.toList . Vector.toList

scoreBoard :: Board -> Integer -> Integer
scoreBoard b justCalled = let
  unmarkedSum = getSum
    $ foldMap (foldMap (\e -> if not (marked e) then Sum (number e) else 0)) b
  in unmarkedSum * justCalled

readBoards :: [String] -> [Board]
readBoards [] = []
readBoards s = let
  (b, rest) = break (== "") s
  board = readBoard b
  in board : readBoards (drop 1 rest)

readBoard :: [String] -> Board
readBoard = Vector.fromList . fmap readRow

readRow :: String -> Row
readRow s = Vector.fromList (go s)
  where
    go :: String -> [Entry]
    go "" = []
    go r = let
      (n, rest) = span isDigit (dropWhile isSpace r)
      in (Entry (read n) False : go rest)

type Board = Vector Row
type Row = Vector Entry
data Entry = Entry
  { number :: Integer
  , marked :: Bool
  } deriving Show
