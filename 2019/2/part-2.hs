import Data.List.Split (splitOn)
import qualified Data.Vector as V
import Control.Category ((>>>))
import Debug.Trace (trace)

main :: IO ()
main = interact (readMemory >>> findInputs >>> show)

findInputs :: State -> Maybe (Int, Int)
findInputs s = let
  possibleInputs = cartesian [1..100] :: [(Int, Int)]
  in foldr (\t r -> if runProgram ((uncurry $ setInputs s) t) == desiredOutput
                        then Just t
                        else r) Nothing possibleInputs

cartesian :: [a] -> [(a, a)]
cartesian xs = (fmap (,) xs) <*> xs

readMemory :: String -> State
readMemory = splitOn "," >>> fmap read >>> V.fromList >>> State 0

desiredOutput :: Int
desiredOutput = 19690720

setInputs :: State -> Int -> Int -> State
setInputs s x y = s { memory = (V.// [(1, x), (2, y)]) (memory s) }

runProgram :: State -> Int
runProgram s = let
  (o, x, y, z, s') = readOp s
  in case runOpCode o x y z s' of
       Left i -> i
       Right s'' -> runProgram s''

data State = State
  { instructionPointer :: Int
  , memory :: V.Vector Int
  } deriving Show

readOp :: State -> (Int, Int, Int, Int, State)
readOp s@(State i v) = (v V.! i,
                        v V.! (v V.! (i + 1)),
                        v V.! (v V.! (i + 2)),
                        v V.! (i + 3),
                        nextOp s)

nextOp :: State -> State
nextOp s = s { instructionPointer = (instructionPointer s) + 4 }

runOpCode :: Int -> Int -> Int -> Int -> State -> Either Int State
runOpCode 1 x y z s = Right $
                      trace ("Adding " <> show x
                      <> " to " <> show y <> ", storing in "
                      <> show z)
                      operation (+) x y z s
runOpCode 2 x y z s = Right $
                      trace ("Multiplying " <> show x
                      <> " by " <> show y <> ", storing in "
                      <> show z)
                      operation (*) x y z s
runOpCode 99 _ _ _ (State _ v) = trace "Halting" Left (v V.! 0)

operation :: (Int -> Int -> Int) -> Int -> Int -> Int -> State -> State
operation f x y z (State i v) = State i (v V.// [(z, f x y)])
