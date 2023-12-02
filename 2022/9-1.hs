module Main where

main :: IO ()
main = interact $ \input -> let
  commands :: [Command]
  commands = fmap readCommand (lines input)

  applyCommand :: Command -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -- Head, Tail
  applyCommand _ (h, t) | h == t = (h, t)
  applyCommand c p@((hx, hy), (tx, ty)) | hx == tx || hy == ty = straight c p
  applyCommand c p = diagonal c p

  straight :: Command -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
  straight = undefined

  step :: Direction -> (Int, Int) -> (Int, Int)
  step Up = second (+ 1)
  step Down = second (- 1)
  step Left = first (- 1)
  step Right = first (+ 1)
  in _

readCommand :: String -> Command
readCommand s = let
  [d, n] = take 2 (words s)
  in Command
    { direction = readDirection d
    , distance = read n
    }

data Command = Command
  { direction :: Direction
  , distance :: Int
  } deriving (Show, Eq)

readDirection :: String -> Direction
readDirection "U" = Up
readDirection "R" = Right
readDirection "D" = Down
readDirection "L" = Left
readDirection _ = error "Unrecognised direction"

data Direction
  = Up
  | Right
  | Down
  | Left
  deriving (Show, Eq)
