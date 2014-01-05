module PXM where

import Data.Array
import Data.Word



data PXM = PBM Header (Array (Int,Int) Bool)
         | PGM Header (Array (Int,Int) Word8)
         | PPM Header (Array (Int,Int) (Word8,Word8,Word8))
         | PGMWide Header (Array (Int,Int) Word16)
         | PPMWide Header (Array (Int,Int) (Word16,Word16,Word16))

instance Show PXM where
    show (PBM (Header idy w h _) x) = "Bitmap: "++show idy++"\n"++
                                      "height: "++show h++"\n"++
                                      "width:  "++show w++"\n"++
                                      "content "++take 80 (show x)
    show (PGM (Header idy w h d) x) = "Greymap: "++show idy++"\n"++
                                      "height:  "++show h++"\n"++
                                      "width:   "++show w++"\n"++
                                      "depth:  "++show d++"\n"++
                                      "content "++take 80 (show x)
    show (PPM (Header idy w h d) x) = "Pixmap: "++show idy++"\n"++
                                      "height: "++show h++"\n"++
                                      "width:  "++show w++"\n"++
                                      "depth:  "++show d++"\n"++
                                      "content "++take 80 (show x)
    show (PGMWide (Header idy w h d) x) = "Greymap: "++show idy++"\n"++
                                          "height:  "++show h++"\n"++
                                          "width:   "++show w++"\n"++
                                          "depth:  "++show d++"\n"++
                                          "content "++take 80 (show x)
 
    show (PPMWide (Header idy w h d) x) = "Pixmap: "++show idy++"\n"++
                                          "height: "++show h++"\n"++
                                          "width:  "++show w++"\n"++
                                          "depth:  "++show d++"\n"++
                                          "content "++take 80 (show x)
                                          
  
data Header = Header Magic Width Height Depth

data Magic = P1 -- Bitmap ascii
           | P2 -- Grayscale ascii
           | P3 -- Pixel ascii
           | P4 -- Bitmap raw
           | P5 -- Grayscale raw
           | P6 -- Pixel raw
           deriving (Show)

type Width  = Int
type Height = Int
type Depth  = Word16
