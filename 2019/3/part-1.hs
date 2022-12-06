module Main where

import Prelude hiding (Left, Right)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List.Split (splitOn)
import Control.Category ((>>>))
import Data.Bifunctor (bimap)

main :: IO ()
main = interact (readInput
                 >>> bimap path' path'
                 >>> uncurry crossings
                 >>> Map.delete (Location 0 0)
                 -- >>> Map.map (manhattan mempty)
                 >>> minimum
                 >>> show)

showSegmentTuple :: ([Segment], [Segment]) -> String
showSegmentTuple = show . bimap showSegments showSegments

showSegments :: [Segment] -> String
showSegments ss = show (head ss) ++ " -> " ++ show (last ss)

data Segment = Segment Direction Integer deriving Show
data Direction = Up | Down | Left | Right deriving Show
type X = Integer
type Y = Integer
type Steps = Integer
data Location = Location X Y deriving (Ord, Eq, Show)

instance Semigroup Location where
  (Location x y) <> (Location x' y') = Location (x + x') (y + y')

instance Monoid Location where
  mempty = Location 0 0

crossings :: Path -> Path -> Map Location Steps
crossings (Path ls _) (Path ls' _) = Map.intersectionWith (+) ls ls'

manhattan :: Location -> Location -> Integer
manhattan (Location x y) (Location x' y') = abs (x - x') + abs (y - y')

-- path :: [Segment] -> Path
-- path = foldMap lineFrom

path' :: [Segment] -> Path
path' = foldl (\(Path ls l) s -> let
                  (Path ls' l') = lineFrom' l (Map.findWithDefault 0 l ls) s
              in Path (Map.union ls ls') l') emptyPath

-- lineFrom :: Segment -> Path
-- lineFrom (Segment d n) = let
--   end = case d of
--     Up -> Location 0 (negate n)
--     Down -> Location 0 n
--     Left -> Location (negate n) 0
--     Right -> Location n 0
--   locations = case d of
--     Up -> fmap (Location 0) [0..(negate n)]
--     Down -> fmap (Location 0) [0..n]
--     Left -> fmap (flip Location 0) [0..(negate n)]
--     Right -> fmap (flip Location 0) [0..n]
--   in Path (Map.fromList locations) end n

lineFrom' :: Location -> Steps -> Segment -> Path
lineFrom' (Location x y) steps (Segment d n) = let
  end = case d of
    Up -> Location x (y - n)
    Down -> Location x (y + n)
    Left -> Location (x - n) y
    Right -> Location (x + n) y
  locations = case d of
    Up -> zip (fmap (Location x) [y, (y - 1)..(y - n)])
              (fmap (+ steps) [0..n])
    Down -> zip (fmap (Location x) [y..(y + n)])
                (fmap (+ steps) [0..n])
    Left -> zip (fmap (flip Location y) [x, pred x..(x - n)])
                (fmap (+ steps) [0..n])
    Right -> zip (fmap (flip Location y) [x..(x + n)])
                 (fmap (+ steps) [0..n])
  in Path (Map.fromList locations) end

readInput :: String -> ([Segment], [Segment])
readInput = lines >>> \[l, l'] -> bimap segmentList segmentList (l, l')

segmentList :: String -> [Segment]
segmentList = splitOn "," >>> fmap segment

segment :: String -> Segment
segment ('U':n) = Segment Up (read n :: Integer)
segment ('D':n) = Segment Down (read n :: Integer)
segment ('L':n) = Segment Left (read n :: Integer)
segment ('R':n) = Segment Right (read n :: Integer)

data Path = Path (Map Location Steps) Location deriving Show

-- instance Semigroup Path where
--   Path ls end <> Path ls' end' =
--     Path (Map.union ls (S.map (end <>) ls')) (end <> end')

emptyPath :: Path
emptyPath = Path Map.empty mempty
