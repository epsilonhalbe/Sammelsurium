module Raw.Wide.Parser where

import PXM
import Util.Parser
import Control.Exception (assert)

import Data.Attoparsec (Parser)
import Data.Word (Word16)


pixel  :: Depth -> Parser (Word16,Word16,Word16)
pixel d = do red   <- anyWord16
             green <- anyWord16
             blue  <- anyWord16
             return (assert (red  <=d) red
                    ,assert (green<=d) green
                    ,assert (blue <=d) blue)

gray :: Depth -> Parser Word16
gray d = do g <- anyWord16
            return $ assert (g <= d) g
