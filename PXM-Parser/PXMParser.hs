module PXMParser where

import PXM
import ASCII.Parser
import ASCII.Wide.Parser
import Raw.Parser
import Raw.Wide.Parser
import Util.Parser

import Control.Applicative ((<$>))
import Data.Array (listArray)
import Data.Attoparsec.Char8 ( Parser
                             , many'
                             , decimal
                             , char
                             , digit)

parsePXM :: Parser PXM
parsePXM = do _ <- comments
              headr@(Header idy w h d) <- parseHeader
              -- dispatching according to magic values
              let mkArray = listArray ((1,1),(h,w))
              case idy of P1 -> do arr <- manyf bool
                                   return $ PBM headr (mkArray arr)
                          P2 -> if d<=256 then do arr <- manyf $ ASCII.Parser.gray d
                                                  return $ PGM headr (mkArray arr)
                                            else do arr <- manyf $ ASCII.Wide.Parser.gray d
                                                    return $ PGMWide headr (mkArray arr)
                          P3 -> if d<=256 then do arr <- manyf $ ASCII.Parser.pixel d
                                                  return $ PPM headr (mkArray arr)
                                            else do arr <- manyf $ ASCII.Wide.Parser.pixel d
                                                    return $ PPMWide headr (mkArray arr)
                          P4 -> do arr <- concat <$> manyf (boolRow w)
                                   return $ PBM headr (mkArray arr)
                          P5 -> if d<=256 then do arr <- manyf $ Raw.Parser.gray d
                                                  return $ PGM headr (mkArray arr)
                                            else do arr <- manyf $ Raw.Wide.Parser.gray d
                                                    return $ PGMWide headr (mkArray arr)
                          P6 -> if d<=256 then do arr <- manyf $ Raw.Parser.pixel d
                                                  return $ PPM headr (mkArray arr)
                                            else do arr <- manyf $ Raw.Wide.Parser.pixel d
                                                    return $ PPMWide headr (mkArray arr)

         where manyf = many' . filtered

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

-- Boolmap Parsing


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

