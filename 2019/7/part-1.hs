{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Prelude hiding (log)

import Data.Coerce (coerce)
import Data.List (permutations)
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
                 >>> runStateT (findBestNumbers)
                 -- >>> runStateT (testAmplifiers (1,0,3,2,4))
                 >>> runIdentity
                 >>> fst
                 -- >>> snd
                 -- >>> (\i -> (executed i, output i))
                 >>> show)


findBestNumbers :: State Int
findBestNumbers = do
  interpreter <- get
  let numbers = permutations [0,1,2,3,4]
  let test (a:b:c:d:e:_) = testAmplifiers (a,b,c,d,e)
  fmap maximum (traverse (\i -> put interpreter >> test i) numbers)


testAmplifiers :: (Int, Int, Int, Int, Int) -> State Int
testAmplifiers (a, b, c, d, e) = do
  initialProgram <- get
  setInput [a, 0]
  (outputA:_) <- runProgram <&> output
  put initialProgram
  setInput [b, outputA]
  (outputB:_) <- runProgram <&> output
  put initialProgram
  setInput [c, outputB]
  (outputC:_) <- runProgram <&> output
  put initialProgram
  setInput [d, outputC]
  (outputD:_) <- runProgram <&> output
  put initialProgram
  setInput [e, outputD]
  (outputE:_) <- runProgram <&> output
  pure outputE

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
initialise v = Interpreter (Address 0) v [] [] []

setInput :: [Int] -> State ()
setInput input = get >>= \i -> put $ i { input = input }

setArguments :: Int -> Int -> State ()
setArguments x y = do
  s <- get
  put s { memory = (V.// [(1, x), (2, y)]) (memory s) }

runProgram :: State Interpreter
runProgram = do
  result <- step
  case trace ("Got result of " <> show result) result of
       Just _ -> trace "Returning" get
       Nothing -> runProgram

type State = StateT Interpreter Identity

data Interpreter = Interpreter
  { instructionPointer :: Address
  , memory :: V.Vector Int
  , executed :: [Operation]
  , output :: [Int]
  , input :: [Int]
  }

newtype Address = Address Int deriving (Show)

instance Semigroup Address where
  (Address x) <> (Address y) = Address (x + y)

instance Show Interpreter where
  show i = "Interpreter" <> "\n"
           <> "( instructionPointer = " <> show (instructionPointer i) <> "\n"
           -- <> ", next = " <> show (fst $ runIdentity $ runStateT peekOp i) <> "\n"
           <> ", executed = " <> show (executed i) <> "\n"
           <> ", output = " <> show (output i) <> "\n"
           <> ", input = " <> show (input i) <> "\n"
           <> ", result = " <> show (memory i V.! 0) <> "\n"
           <> ")" <> "\n"

data ParameterMode = Position | Immediate deriving Show
data Parameter = Parameter ParameterMode Int deriving Show

data Operation
  = Add Parameter Parameter Address
  | Multiply Parameter Parameter Address
  | Read Address
  | Write Parameter
  | Halt
  | JumpTrue Parameter Parameter
  | JumpFalse Parameter Parameter
  | LessThan Parameter Parameter Address
  | Equals Parameter Parameter Address
  | UnrecognisedOpCode Int
  deriving Show

step :: State (Maybe Int)
step = readOp >>= \i -> recordInstruction i
                        >> runOperation i

readMode :: Int -> ParameterMode
readMode 0 = Position
readMode 1 = Immediate

readOp :: State Operation
readOp = do
  opCode <- read1
  let modes = fmap readMode $ drop 2 (reverseDigits opCode) ++ repeat 0
  let opType = mod opCode 100
  case opType of
        1 -> Add <$> readParameter (modes !! 0)
                 <*> readParameter (modes !! 1)
                 <*> readAddress
        2 -> Multiply <$> readParameter (modes !! 0)
                      <*> readParameter (modes !! 1)
                      <*> readAddress
        3 -> Read <$> readAddress
        4 -> Write <$> readParameter (modes !! 0)
        5 -> JumpTrue <$> readParameter (modes !! 0)
                      <*> readParameter (modes !! 1)
        6 -> JumpFalse <$> readParameter (modes !! 0)
                       <*> readParameter (modes !! 1)
        7 -> LessThan <$> readParameter (modes !! 0)
                      <*> readParameter (modes !! 1)
                      <*> readAddress
        8 -> Equals <$> readParameter (modes !! 0)
                    <*> readParameter (modes !! 1)
                    <*> readAddress
        99 -> pure Halt
        n -> pure $ UnrecognisedOpCode n

peekOp :: State Operation
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
  let result = memory i V.! coerce (instructionPointer i)
  pure result

readAt :: Int -> State Int
readAt i = do
  interpreter <- get
  pure $ memory interpreter V.! i

readRelative :: Int -> State Int
readRelative i = do
  interpreter <- get
  pure $ memory interpreter V.! ((coerce $ instructionPointer interpreter) + i)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

seek :: Int -> State ()
seek n = do
  modify (\i -> i { instructionPointer = instructionPointer i <> Address n })

read1 :: State Int
read1 = readCurrent <* seek 1

readParameter :: ParameterMode -> State Parameter
readParameter m = (readCurrent <&> Parameter m) <* seek 1

readAddress :: State Address
readAddress = (readCurrent <&> Address) <* seek 1

read2 :: State (Int, Int)
read2 = do
  interpreter <- get
  let v = memory interpreter
  let i = instructionPointer interpreter
  seek 2
  pure $ (,) (v V.! coerce i) (v V.! (coerce i + 1))


read3 :: State (Int, Int, Int)
read3 = do
  interpreter <- get
  let v = memory interpreter
  let i = instructionPointer interpreter
  seek 3
  pure $ (,,) (v V.! coerce i) (v V.! (coerce i + 1)) (v V.! (coerce i + 2))

use2 :: (Int -> Int -> a) -> State a
use2 f = do
  (x, z) <- read2
  pure $ f x z

use3 :: (Int -> Int -> Address -> a) -> State a
use3 f = do
  (x, y, z) <- read3
  pure $ f x y (Address z)

write :: Int -> Address -> State ()
write x a = do
  interpreter <- trace ("Writing " <> show x <> " to " <> show a) get
  let v' = memory interpreter V.// [(coerce a, x)]
  put (interpreter { memory = v' })

recordInstruction :: Operation -> State ()
recordInstruction i = do
  interpreter <- get
  put interpreter { executed = i : executed interpreter }

resolveParameter :: Parameter -> State Int
resolveParameter (Parameter Immediate i) = pure i
resolveParameter (Parameter Position i) = trace ("Reading from " <> show i) readAt i

runOperation :: Operation -> State (Maybe Int)
runOperation (Add x y z) = binaryOperation (+) x y z
                                            $> Nothing
runOperation (Multiply x y z) = binaryOperation (*) x y z
                                                 $> Nothing
runOperation (LessThan x y z) = let lt x y = bool 0 1 (x < y)
                                                 in binaryOperation lt x y z
                                                 $> Nothing
runOperation (Equals x y z) = let eq x y = bool 0 1 (x == y)
                                                 in binaryOperation eq x y z
                                                 $> Nothing
runOperation Halt = trace "Running halt operation" Just <$> readAt 0
runOperation (Write x) = do
  x' <- resolveParameter x
  modify (\i -> i { output = x' : output i })
  pure Nothing
runOperation (Read a) = do
  gets (input >>> head) >>= flip write a
  modify (\i -> i { input = tail $ input i })
  pure Nothing
runOperation (JumpTrue x a) = do
  x' <- resolveParameter x
  a' <- resolveParameter a
  if x' /= 0
    then modify (\i -> i { instructionPointer = Address a' })
    else pure ()
  pure Nothing
runOperation (JumpFalse x a) = do
  x' <- resolveParameter x
  a' <- resolveParameter a
  if x' == 0
    then modify (\i -> i { instructionPointer = Address a' })
    else pure ()
  pure Nothing

binaryOperation :: (Int -> Int -> Int) -> Parameter -> Parameter -> Address -> State ()
binaryOperation f x y z = do
  [x', y'] <- traverse resolveParameter [x, y]
  let result = trace ("Combined " <> show x <> " and "
                      <> show y <> " to get " <> show (f x' y'))
               (f x' y')
  write result z
