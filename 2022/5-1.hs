module Main where
import Data.List (transpose)
import Data.Char (isSpace)
import Control.Lens hiding (indices)

main :: IO ()
main = interact $ \input -> let
  ls = lines input
  (drawing, "" : procedure) = break null ls
  columns = transpose drawing
  indices = [1, 5 .. length columns]
  stacks = fmap (dropWhile isSpace . (columns !!)) indices
  resultStacks = run stacks (fmap readCommand procedure)
  in fmap head resultStacks

run :: [[Char]] -> [Command] -> [[Char]]
run stacks [] = stacks
run stacks (Move n sourceIndex targetIndex : cs) = let
  source = stacks !! sourceIndex
  newStacks = stacks
    & (ix sourceIndex %~ tail)
    . (ix targetIndex %~ (head source :))
  c' = if n == 1 then Nothing else Just (Move (n - 1) sourceIndex targetIndex)
  in run newStacks (maybe cs (: cs) c')

data Command = Move Integer Int Int

readCommand :: String -> Command
readCommand s = let
  command = words s
  n = read (command !! 1)
  source = read (command !! 3) - 1
  target = read (command !! 5) - 1
  in Move n source target
