module ASCII.Wide.Parser where

import PXM
import Util.Parser
import Control.Exception (assert)

import Data.Attoparsec.Char8 (Parser, decimal)
import Data.Word (Word16)

gray :: Depth -> Parser Word16
gray d = do g <- decimal
            return $ assert (g <= d) g

pixel :: Depth -> Parser (Word16, Word16, Word16)
pixel d = do red   <- decimal
             _     <- whiteSpaces
             green <- decimal
             _     <- whiteSpaces
             blue  <- decimal
             _     <- whiteSpaces
             return (assert (red  <=d) red
                    ,assert (green<=d) green
                    ,assert (blue <=d) blue)
