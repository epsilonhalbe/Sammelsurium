module ASCII.Parser where

import PXM
import Util.Parser

import Control.Exception (assert)
import Control.Applicative ((<|>))
import Data.Attoparsec.Char8 ( Parser
                             , char
                             , decimal)
import Data.Word (Word8)

bool :: Parser Bool
bool = do c <- char '1' <|>  char '0'
          case c of '1' -> return True
                    '0' -> return False
                    _   -> error "neither '0' nor '1' character in bitmap"


gray :: Depth -> Parser Word8
gray d = do let d' = fromIntegral d
            g <- decimal
            return $ assert (g <= d') g

pixel :: Depth -> Parser (Word8, Word8, Word8)
pixel d = do let d' = fromIntegral d :: Word8
             red   <- decimal
             _     <- whiteSpaces
             green <- decimal
             _     <- whiteSpaces
             blue  <- decimal
             _     <- whiteSpaces
             return ( assert (red  <=d') red
                    , assert (green<=d') green
                    , assert (blue <=d') blue)

