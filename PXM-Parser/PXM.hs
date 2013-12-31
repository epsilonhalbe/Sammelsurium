module PXM where

import Data.Array


data PXM = PBM Header (Array (Int,Int) Bit)
         | PGM Header (Array (Int,Int) Grayscale)
         | PPM Header (Array (Int,Int) Pixel)
         deriving (Show)

data Header = Header Magic Width Height Depth
            deriving (Show)

data Magic = P1 -- Bitmap ascii
           | P2 -- Grayscale ascii
           | P3 -- Pixel ascii
           | P4 -- Bitmap raw
           | P5 -- Grayscale raw
           | P6 -- Pixel raw
           deriving (Show)

type Width  = Int
type Height = Int
type Depth  = Int

type Bit = Bool

type Grayscale = Int

type Red   = Int
type Green = Int
type Blue  = Int
data Pixel = Pixel Red Green Blue
           deriving (Show)
