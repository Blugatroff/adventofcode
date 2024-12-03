{-# LANGUAGE ScopedTypeVariables #-}

module Year2021.Day16 (partOne, partTwo) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (MonadState (put), State, evalState, get, gets, lift, modify)
import Control.Monad.Trans.Except (throwE)
import Data.Bits (shiftL, (.|.))
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.List (foldl')
import Util (safeHead)

data Bit = Zero | One

instance Show Bit where
  show Zero = "0"
  show One = "1"

newtype Bits = Bits {bits :: [Bit]}

instance Show Bits where
  show = foldr f "" . bits
    where
      f :: Bit -> String -> String
      f One s = '1' : s
      f Zero s = '0' : s

instance Semigroup Bits where
  a <> b = Bits $ bits a <> bits b

hexadecimalToBinary :: Char -> Bits
hexadecimalToBinary '0' = Bits [Zero, Zero, Zero, Zero]
hexadecimalToBinary '1' = Bits [Zero, Zero, Zero, One]
hexadecimalToBinary '2' = Bits [Zero, Zero, One, Zero]
hexadecimalToBinary '3' = Bits [Zero, Zero, One, One]
hexadecimalToBinary '4' = Bits [Zero, One, Zero, Zero]
hexadecimalToBinary '5' = Bits [Zero, One, Zero, One]
hexadecimalToBinary '6' = Bits [Zero, One, One, Zero]
hexadecimalToBinary '7' = Bits [Zero, One, One, One]
hexadecimalToBinary '8' = Bits [One, Zero, Zero, Zero]
hexadecimalToBinary '9' = Bits [One, Zero, Zero, One]
hexadecimalToBinary 'A' = Bits [One, Zero, One, Zero]
hexadecimalToBinary 'B' = Bits [One, Zero, One, One]
hexadecimalToBinary 'C' = Bits [One, One, Zero, Zero]
hexadecimalToBinary 'D' = Bits [One, One, Zero, One]
hexadecimalToBinary 'E' = Bits [One, One, One, Zero]
hexadecimalToBinary 'F' = Bits [One, One, One, One]
hexadecimalToBinary _ = Bits []

hexStringToBits :: String -> Bits
hexStringToBits = Bits . (>>= (bits . hexadecimalToBinary))

parse :: String -> Bits
parse = hexStringToBits

data OperatorType = Sum | Product | Minimum | Maximum | Greater | Less | Equal
  deriving (Show)

data Packet = Literal !Int !Int | Operator !OperatorType !Int ![Packet]
  deriving (Show)

takeN :: Int -> State Bits Bits
takeN n = do
  b <- gets $ Bits . take n . bits
  modify (Bits . (drop n . bits))
  return b

takeOne :: State Bits (Maybe Bit)
takeOne = safeHead . bits <$> takeN 1

parseLiteralBits :: ExceptT String (State Bits) Bits
parseLiteralBits = do
  ty <- lift takeOne
  case ty of
    Just Zero -> lift $ takeN 4
    Just One -> do
      b <- lift $ takeN 4
      parseLiteralBits <&> (b <>)
    Nothing -> throwE "expected literal type bit"

bitsToInt :: Bits -> Int
bitsToInt b = foldl' f 0 (bits b)
  where
    f :: Int -> Bit -> Int
    f n One = shiftL n 1 .|. 1
    f n Zero = shiftL n 1

parseLiteral :: Int -> ExceptT String (State Bits) Packet
parseLiteral version = Literal version . bitsToInt <$> parseLiteralBits

parseOperatorA :: OperatorType -> Int -> ExceptT String (State Bits) Packet
parseOperatorA opType version = do
  length <- lift $ takeN 11
  subPackets <- sequence $ [1 .. bitsToInt length] $> parsePacket
  return $ Operator opType version subPackets

parseManyPackets :: ExceptT String (State Bits) [Packet]
parseManyPackets =
  gets bits >>= \case
    [] -> return []
    rest -> do
      packet <- parsePacket
      parseManyPackets <&> (packet :)

parseOperatorB :: OperatorType -> Int -> ExceptT String (State Bits) Packet
parseOperatorB opType version = do
  length <- lift $ bitsToInt <$> takeN 15
  bts <- get
  put (Bits $ take length $ bits bts)
  subPackets <- parseManyPackets
  modify (\rest -> Bits $ bits rest <> drop length (bits bts))
  return $ Operator opType version subPackets

parseOperator :: OperatorType -> Int -> ExceptT String (State Bits) Packet
parseOperator opType version = do
  lift takeOne >>= \case
    Just One -> parseOperatorA opType version
    Just Zero -> parseOperatorB opType version
    Nothing -> throwE "Expected length type ID"

parseOperatorType :: ExceptT String (State Bits) (Maybe OperatorType)
parseOperatorType = do
  takeN 3 <&> bitsToInt & lift >>= \case
    0 -> return $ Just Sum
    1 -> return $ Just Product
    2 -> return $ Just Minimum
    4 -> return Nothing
    3 -> return $ Just Maximum
    5 -> return $ Just Greater
    6 -> return $ Just Less
    7 -> return $ Just Equal
    _ -> get >>= \bits -> throwE $ "failed to parse operator type: " <> show bits

parsePacket :: ExceptT String (State Bits) Packet
parsePacket = do
  version <- takeN 3 <&> bitsToInt & lift
  parseOperatorType >>= \case
    Nothing -> parseLiteral version
    Just opType -> parseOperator opType version

evalPacket :: Packet -> Int
evalPacket (Literal _ value) = value
evalPacket (Operator opType _ subs) = case opType of
  Sum -> subs <&> evalPacket & sum
  Product -> subs <&> evalPacket & product
  Minimum -> subs <&> evalPacket & minimum
  Maximum -> subs <&> evalPacket & maximum
  Greater -> subs <&> evalPacket & binary (>)
  Less -> subs <&> evalPacket & binary (<)
  Equal -> subs <&> evalPacket & binary (==)
  where
    binary :: (Int -> Int -> Bool) -> [Int] -> Int
    binary f [a, b] = if a `f` b then 1 else 0
    binary f _ = 0

packetVersionSum :: Packet -> Int
packetVersionSum (Operator opType version subPackets) = version + (subPackets <&> packetVersionSum & sum)
packetVersionSum (Literal version _) = version

solvePartOne :: Bits -> Either String Int
solvePartOne bits = evalState (runExceptT parsePacket) bits <&> packetVersionSum

solvePartTwo :: Bits -> Either String Int
solvePartTwo bits = evalState (runExceptT parsePacket) bits <&> evalPacket

partOne :: String -> Either String String
partOne input = parse input & solvePartOne <&> show

partTwo :: String -> Either String String
partTwo input = parse input & solvePartTwo <&> show
