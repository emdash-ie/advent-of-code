import Data.List.Split (splitOn)
import qualified Data.Vector as V
import Control.Category ((>>>))
import Debug.Trace (trace)

main :: IO ()
main = interact (show . solution . readInput)

readInput :: String -> State
readInput = (splitOn ",") >>> fmap read >>> V.fromList
            >>> (V.// [(1, 12), (2, 2)]) >>> State 0

solution :: State -> Int
solution s = let
  (o, x, y, z, s') = readOp s
  in case runOpCode o x y z s' of
       Left i -> i
       Right s'' -> solution s''

data State = State Int (V.Vector Int) deriving Show

readOp :: State -> (Int, Int, Int, Int, State)
readOp s@(State i v) = (v V.! i,
                        v V.! (v V.! (i + 1)),
                        v V.! (v V.! (i + 2)),
                        v V.! (i + 3),
                        nextOp s)

nextOp :: State -> State
nextOp (State i v) = State (i + 4) v

runOpCode :: Int -> Int -> Int -> Int -> State -> Either Int State
runOpCode 1 x y z s = Right $ trace ("Adding " <> show x
                      <> " to " <> show y <> ", storing in "
                      <> show z)
                      operation (+) x y z s
runOpCode 2 x y z s = Right $ trace ("Multiplying " <> show x
                      <> " by " <> show y <> ", storing in "
                      <> show z)
                      operation (*) x y z s
runOpCode 99 _ _ _ (State _ v) = trace "Halting" Left (v V.! 0)

operation :: (Int -> Int -> Int) -> Int -> Int -> Int -> State -> State
operation f x y z (State i v) = State i (v V.// [(z, f x y)])
