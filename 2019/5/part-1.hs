import Prelude hiding (log)

import Data.Coerce (coerce)
import Data.List.Split (splitOn)
import qualified Data.Vector as V
import Control.Category ((>>>))
import Debug.Trace (trace)
import Control.Monad ((>=>), zipWithM, replicateM)
import Control.Monad.Trans.State (StateT(..), get, gets, put, modify)
import Data.Functor.Identity (Identity(..))
import Data.Functor ((<&>), ($>))
import Data.Bool (bool)

main :: IO ()
main = interact (readMemory
                 >>> runStateT (setInput [1] >> runDebug)
                 >>> runIdentity
                 >>> fst
                 -- >>> snd
                 -- >>> (\i -> (executed i, output i))
                 >>> show)

runDebug :: State [Interpreter]
runDebug = do
  interpreter <- get
  fmap (interpreter :) go
  where
    go = do
      result <- step
      interpreter <- get
      case result of
        Nothing -> fmap (interpreter :) go
        Just _ -> pure [interpreter]

runDiagnostic :: State [Int]
runDiagnostic = setInput [1] >> runProgram >> get <&> output

readMemory :: String -> Interpreter
readMemory = splitOn "," >>> fmap read >>> V.fromList >>> initialise

initialise :: V.Vector Int -> Interpreter
initialise v = Interpreter 0 v [] [] []

setInput :: [Int] -> State ()
setInput input = get >>= \i -> put $ i { input = input }

setArguments :: Int -> Int -> State ()
setArguments x y = do
  s <- get
  put s { memory = (V.// [(1, x), (2, y)]) (memory s) }

runProgram :: State Int
runProgram = do
  result <- step
  case trace ("Got result of " <> show result) result of
       Just i -> trace "Returning" pure i
       Nothing -> runProgram

type State = StateT Interpreter Identity

data Interpreter = Interpreter
  { instructionPointer :: Int
  , memory :: V.Vector Int
  , executed :: [Instruction]
  , output :: [Int]
  , input :: [Int]
  }

newtype Address = Address Int deriving Show

instance Show Interpreter where
  show i = "Interpreter" <> "\n"
           <> "( instructionPointer = " <> show (instructionPointer i) <> "\n"
           -- <> ", next = " <> show (fst $ runIdentity $ runStateT peekOp i) <> "\n"
           <> ", executed = " <> show (executed i) <> "\n"
           <> ", output = " <> show (output i) <> "\n"
           <> ", input = " <> show (input i) <> "\n"
           <> ")" <> "\n"

data ParameterMode = Position | Immediate deriving Show
data Instruction = Instruction Operation [ParameterMode]

instance Show Instruction where
  show (Instruction op ms) = "(Instruction " <> show op <> " " <> show (take 4 ms) <> ")"

data Operation
  = Add Int Int Address
  | Multiply Int Int Address
  | Read Address
  | Write Int
  | Halt
  | JumpTrue Int Address
  | JumpFalse Int Address
  | LessThan Int Int Address
  | Equals Int Int Address
  deriving Show

step :: State (Maybe Int)
step = readOp >>= \i -> recordInstruction i
                        >> runOperation i

readMode :: Int -> ParameterMode
readMode 0 = Position
readMode 1 = Immediate

readOp :: State Instruction
readOp = do
  opCode <- read1
  let modes = fmap readMode $ drop 2 (reverseDigits opCode) ++ repeat 0
  let opType = mod opCode 100
  operation <- case opType of
        1 -> use3 Add
        2 -> use3 Multiply
        3 -> (Address >>> Read) <$> read1
        4 -> Write <$> read1
        5 -> use2 JumpTrue
        6 -> use2 JumpFalse
        7 -> use3 LessThan
        8 -> use3 Equals
        99 -> pure Halt
        n -> fail ("Unrecognised opCode " <> show n)
  pure $ Instruction operation modes

peekOp :: State Instruction
peekOp = do
  interpreter <- get
  instruction <- readOp
  put interpreter
  pure instruction

reverseDigits :: Integral a => a -> [a]
reverseDigits = go
  where
    go 0 = []
    go n = mod n 10 : go (div n 10)

readCurrent :: State Int
readCurrent = do
  i <- get
  let result = memory i V.! instructionPointer i
  pure result

readAt :: Int -> State Int
readAt i = do
  interpreter <- get
  pure $ memory interpreter V.! i

readRelative :: Int -> State Int
readRelative i = do
  interpreter <- get
  pure $ memory interpreter V.! ((instructionPointer interpreter) + i)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

seek :: Int -> State ()
seek n = do
  interpreter <- get
  put (interpreter { instructionPointer = instructionPointer interpreter + n })

read1 :: State Int
read1 = readCurrent <* seek 1

read2 :: State (Int, Int)
read2 = do
  interpreter <- get
  let v = memory interpreter
  let i = instructionPointer interpreter
  seek 2
  pure $ (,) (v V.! i) (v V.! (i + 1))


read3 :: State (Int, Int, Int)
read3 = do
  interpreter <- get
  let v = memory interpreter
  let i = instructionPointer interpreter
  seek 3
  pure $ (,,) (v V.! i) (v V.! (i + 1)) (v V.! (i + 2))

use2 :: (Int -> Address -> a) -> State a
use2 f = do
  (x, z) <- read2
  pure $ f x (Address z)

use3 :: (Int -> Int -> Address -> a) -> State a
use3 f = do
  (x, y, z) <- read3
  pure $ f x y (Address z)

write :: Int -> Address -> State ()
write x a = do
  interpreter <- trace ("Writing " <> show x <> " to " <> show a) get
  let v' = memory interpreter V.// [(coerce a, x)]
  put (interpreter { memory = v' })

recordInstruction :: Instruction -> State ()
recordInstruction i = do
  interpreter <- get
  put interpreter { executed = i : executed interpreter }

maybeLookup :: ParameterMode -> Int -> State Int
maybeLookup Immediate i = pure i
maybeLookup Position i = trace ("Reading from " <> show i) readAt i

runOperation :: Instruction -> State (Maybe Int)
runOperation (Instruction (Add x y z) ms) = binaryOperation (+) [x,y] z ms
                                            $> Nothing
runOperation (Instruction (Multiply x y z) ms) = binaryOperation (*) [x,y] z ms
                                                 $> Nothing
runOperation (Instruction (LessThan x y z) ms) = let lt x y = bool 0 1 (x < y)
                                                 in binaryOperation lt [x,y] z ms
                                                 $> Nothing
runOperation (Instruction (Equals x y z) ms) = let eq x y = bool 0 1 (x == y)
                                                 in binaryOperation eq [x,y] z ms
                                                 $> Nothing
runOperation (Instruction Halt _) = trace "Running halt operation" Just <$> readAt 0
runOperation (Instruction (Write x) ms) = do
  interpreter <- get
  x' <- maybeLookup (head ms) x
  put $ interpreter { output = x' : output interpreter }
  pure Nothing
runOperation (Instruction (Read a) _) = do
  interpreter <- get
  write (head $ input interpreter) a
  interpreter <- get
  put interpreter { input = tail $ input interpreter }
  pure Nothing
runOperation (Instruction (JumpTrue _ _) _) = fail "Unimplemented JumpTrue"
runOperation (Instruction (JumpFalse _ _) _) = fail "Unimplemented JumpFalse"

binaryOperation :: (Int -> Int -> Int) -> [Int] -> Address -> [ParameterMode] -> State ()
binaryOperation f args z modes = do
  [x, y] <- lookupParameters modes args
  let result = trace ("Combined " <> show x <> " and "
                      <> show y <> " to get " <> show (f x y))
               (f x y)
  write result z

lookupParameters :: [ParameterMode] -> [Int] -> State [Int]
lookupParameters = zipWithM maybeLookup
