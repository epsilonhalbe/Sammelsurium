module Util.Parser where

import Control.Applicative ((<|>))
import Data.ByteString
import Data.Attoparsec.ByteString as B
import Data.Attoparsec.Char8
import Data.Bits
import Data.Word


anyWord16 :: Parser Word16
anyWord16 = do hi <- anyWord8
               lo <- anyWord8
               let hi' = fromIntegral hi `shiftL` 8 :: Word16
                   lo' = fromIntegral lo :: Word16
               return $ hi'+lo'

whiteSpaces :: Parser String
whiteSpaces = many' whiteSpace

whiteSpace :: Parser Char
whiteSpace = char ' ' <|> char '\r' <|> char '\n'

comments :: Parser [ByteString]
comments = many' comment

comment :: Parser ByteString
comment = do _ <- whiteSpace
             _ <- char '#'
             B.takeTill isEndOfLine

filtered :: Parser a -> Parser a
filtered prsr = do _ <- whiteSpaces
                   prsr

