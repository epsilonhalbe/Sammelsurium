{-# OPTIONS_GHC
  -fno-warn-unused-binds
  -fno-warn-unused-matches
#-}

module Main where

import Control.Applicative ((<$>))
--import Control.Arrow
--import qualified Data.ByteString.Lazy as B
--import qualified Prelude as P
--import Data.Word

main :: IO ()
main = do n <- read <$> getLine :: IO Int
          input <- map (map read . words) . lines <$> getContents ::IO [[Int]]
          return ()

-- helper stuff
(∈) :: (Eq a) => a -> [a] -> Bool
(∈) = elem
