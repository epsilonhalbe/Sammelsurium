module Raw.Parser where

import PXM

import Data.Attoparsec ( Parser
                       , anyWord8
                       , takeWhile1)
import Data.Attoparsec.Char8 (isEndOfLine)
import qualified Data.ByteString as BS
import Control.Applicative ((<$>))
import Control.Exception (assert)
import Data.Word (Word8)
import Data.Bits (testBit)

pixel  :: Depth -> Parser (Word8,Word8,Word8)
pixel d = do let d' = fromIntegral d
             red   <- anyWord8
             green <- anyWord8
             blue  <- anyWord8
             return (assert (red  <=d') red
                    ,assert (green<=d') green
                    ,assert (blue <=d') blue)

gray :: Depth -> Parser Word8
gray d = do let d' = fromIntegral d
            g <- anyWord8
            return $ assert (g <= d') g

boolRow :: Width -> Parser [Bool]
boolRow w = do content <- BS.unpack . BS.take w <$> takeWhile1 (not . isEndOfLine)
               return $ concatMap f content
          where f :: Word8 -> [Bool]
                f x = map (testBit x) [0..7]
