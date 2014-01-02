module Main where

import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec
import Data.Either (rights)
import Data.List (minimumBy)
import Data.Ord

data Defi = Defi { name :: String
                 , adr  :: String
                 , tel  :: String
                 , lon  :: Double
                 , lat  :: Double
                 }

instance Show Defi where
    show = name

main :: IO ()
main = do lon <- readDouble <$> getLine :: IO Double
          lat <- readDouble <$> getLine :: IO Double
          n   <- read <$> getLine :: IO Int
          defis <- rights . map (parse defi "Parse Error") . lines <$> getContents :: IO [Defi]
          print $ minimumBy (comparing (dist (lon,lat))) defis
          return ()

dist :: (Double, Double) -> Defi -> Double
dist (lon, lat) (Defi _ _ _ lon' lat') = let d1 = abs (lon - lon') * cos ((lat + lat')/2)
                                             d2 = abs (lat - lat')
                                         in sqrt (d1^2 + d2^2) * 6371

defi :: GenParser Char s Defi
defi = do skipMany1 digit
          char ';'
          nam <- many (noneOf ";")
          char ';'
          adr <- many (noneOf ";")
          char ';'
          tel <- many (noneOf ";")
          char ';'
          lon <- many (noneOf ";")
          char ';'
          lat <- many (noneOf ";")
          return $ Defi nam adr tel (readDouble lon) (readDouble lat)

readDouble :: String -> Double
readDouble str = read $ map subst str

subst :: Char -> Char
subst ',' = '.'
subst x   =  x
