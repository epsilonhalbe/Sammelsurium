module Main where

import Control.Applicative ((<$>))
import Data.Char
import Data.Bits
import Data.List (group)

newtype Bit = Bit Bool deriving (Eq)

instance Show Bit where
  show (Bit True) = "0"
  show (Bit False) = "00"

main :: IO ()
main = do input <- group . concatMap (reverse . bitsTo7Bit . ord) <$> getLine ::IO [[Bit]]
          let output = concatMap (\x-> [show $ head x, replicate (length x) '0']) input
          putStrLn $ unwords output
          return ()

bitsTo7Bit :: Bits a => a -> [Bit]
bitsTo7Bit x = take 7 $ bitsToList x ++ repeat (Bit False)

bitsToList :: Bits a => a -> [Bit]
bitsToList x = if x == zero then []
                            else Bit (testBit x 0): bitsToList (x `shiftR` 1)

zero :: Bits a => a
zero = bit 1 `xor` bit 1
