{-# OPTIONS_GHC
  -fno-warn-unused-binds
  -fno-warn-unused-matches
#-}

module Main where

import Control.Applicative ((<$>))
--import Control.Arrow
--import qualified Data.ByteString.Lazy as B
--import qualified Prelude as P
--import Data.Word
import Data.Char
import Data.Bits
import Data.List (group)

newtype Bit = Bit Bool deriving (Eq)

instance Show Bit where
  show (Bit True) = "0"
  show (Bit False) = "00"

main :: IO ()
main = do input <- group . concatMap (reverse . bitsToList . ord) <$> getLine ::IO [[Bit]]
          let output = concatMap (\x-> [show $ head x, replicate (length x) '0']) input
          putStrLn $ unwords output
          return ()

bitsToList :: Bits a => a -> [Bit]
bitsToList x = if x == zero then []
                            else Bit (testBit x 0): bitsToList (x `shiftR` 1)

zero :: Bits a => a
zero = bit 1 `xor` bit 1

-- helper stuff
(∈) :: (Eq a) => a -> [a] -> Bool
(∈) = elem
