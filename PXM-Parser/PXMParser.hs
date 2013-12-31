module PXMParser where

import PXM
import Control.Applicative ((<|>))
import Data.Array
import Data.ByteString
import Data.Attoparsec.Char8
import qualified Data.Attoparsec.ByteString as B
import Data.Word

parsePXM :: Parser PXM
parsePXM = do _ <- comments
              headr@(Header idy w h d) <- parseHeader
              -- dispatching according to magic values
              let mkArray = listArray ((1,1),(h,w))
              case idy of P1 -> do arr <- parsePBMascii
                                   return $ PBM headr (mkArray arr)
                          P2 -> do arr <- parsePGMascii d
                                   return $ PGM headr (mkArray arr)
                          P3 -> do arr <- parsePPMascii d
                                   return $ PPM headr (mkArray arr)
                          P4 -> do arr <- parsePBMraw
                                   return $ PBM headr (mkArray arr)
                          P5 -> do arr <- parsePGMraw d
                                   return $ PGM headr (mkArray arr)
                          P6 -> do arr <- parsePPMraw d
                                   return $ PPM headr (mkArray arr)

parseHeader :: Parser Header
parseHeader = do _ <- comments
                 idy <- magic
                 _ <- comments
                 _ <- whiteSpaces
                 width <- decimal
                 _ <- whiteSpaces
                 _ <- comments
                 height <- decimal
                 _ <- whiteSpaces
                 _ <- comments
                 depth <- case idy of P1 -> return 1
                                      P4 -> return 1
                                      _  -> do depth <- decimal
                                               _ <- whiteSpaces
                                               _ <- comments
                                               return depth
                 return $ Header idy width height depth

-- Bitmap Parsing
parsePBMascii :: Parser [Bit]
parsePBMascii = many' (filtered bit)

parsePBMraw :: Parser [Bit]
parsePBMraw = undefined

bit :: Parser Bit
bit = do c <- char '1' <|>  char '0'
         case c of '1' -> return True
                   '0' -> return False
                   _   -> error "neither '0' nor '1' character in bitmap"

parsePGMascii :: Depth ->  Parser [Grayscale]
parsePGMascii d = many' (filtered $ grayscale d)

parsePGMraw :: Depth ->  Parser [Grayscale]
parsePGMraw d = many' (filtered $ rawGrayscale d)

grayscale :: Depth -> Parser Grayscale
grayscale d = do g <- decimal
                 bounded d g

rawGrayscale :: Depth -> Parser Grayscale
rawGrayscale d = if d < 256 then do g <- B.anyWord8
                                    bounded d $ fromIntegral g
                            else do g <- anyWord16
                                    bounded d $ fromIntegral g

parsePPMascii :: Depth ->  Parser [Pixel]
parsePPMascii d = many' (filtered $ pixel d)

parsePPMraw :: Depth ->  Parser [Pixel]
parsePPMraw d = many' (filtered $ rawPixel d)

rawPixel :: Depth -> Parser Pixel
rawPixel d = if d < 256 then do red   <- bounded d . fromIntegral =<< B.anyWord8
                                green <- bounded d . fromIntegral =<< B.anyWord8
                                blue  <- bounded d . fromIntegral =<< B.anyWord8
                                return $ Pixel red green blue
                        else do red   <- bounded d . fromIntegral =<< anyWord16
                                green <- bounded d . fromIntegral =<< anyWord16
                                blue  <- bounded d . fromIntegral =<< anyWord16
                                return $ Pixel red green blue

pixel :: Depth -> Parser Pixel
pixel d = do red   <- bounded d =<< decimal
             _     <- whiteSpaces
             green <- bounded d =<< decimal
             _     <- whiteSpaces
             blue  <- bounded d =<< decimal
             _     <- whiteSpaces
             return $ Pixel red green blue

magic :: Parser Magic
magic = do _ <- char 'P'
           idy <- digit
           case idy of '1' -> return P1
                       '2' -> return P2
                       '3' -> return P3
                       '4' -> return P4
                       '5' -> return P5
                       '6' -> return P6
                       _   -> error "no valid magic value"

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

bounded :: (Monad m, Show a, Ord a) => a -> a -> m a
bounded d x = if x<=d then return x
                      else error $ "Value out of bounds!\n"++
                                   "Expected less or equal than "++show d++
                                   ", but got "++show x

filtered :: Parser a -> Parser a
filtered prsr = do _ <- whiteSpaces
                   prsr

anyWord16 :: Parser Word16
anyWord16 = do hi <-B.anyWord8
               lo <- B.anyWord8
               let hi' = 256 * fromIntegral hi :: Word16
                   lo' = fromIntegral lo :: Word16
               return $ hi' + lo'

