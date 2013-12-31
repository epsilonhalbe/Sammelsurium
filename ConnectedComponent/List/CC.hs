import Debug.Trace

import Control.Applicative ((<$>))
import Data.List ((\\),union, foldl')
import Data.Ix (range)

type Point = (Int,Int)
type CC = [Point] --CC stands for connected component

main :: IO ()
main = do [imax,jmax] <- map read . words <$> getLine ::IO [Int]
          let indices = range ((1,1),(imax,jmax))
          ones <- map fst . filter (('1'==).snd) . zip indices . filter (/='\n') <$> getContents
          print . length $ calculateCC [] [] [] ones
          return ()

calculateCC :: [CC] -> CC -> [Point] -> [Point] -> [CC]
-- calculateCC ccs cc nbs base -> ccs
calculateCC ccs cc nbs [] = (nbs++cc):ccs
calculateCC ccs [] [] (b:base) = calculateCC ccs [] [b] base
calculateCC ccs cc [] (b:base) = calculateCC (cc:ccs) [] [b] base
calculateCC ccs cc nbs base = -- let nb x = filter (\b -> dist x b == 1) base
                              let nbs'  = unions (map (`nb` base) nbs) \\ nbs
                                  base' = base \\ nbs'
                              in calculateCC ccs (nbs++cc) nbs' base'

nb :: Point -> [Point] -> [Point]
nb (x,y) base = filter (`elem` base) [                  (x+1,y-1)
                                     ,                  (x+1,y  )
                                     ,(x-1,y+1),(x,y+1),(x+1,y+1)]

{-dist :: Point -> Point -> Int-}
{-dist (x,y) (x',y') = let dx = abs (x-x')-}
                         {-dy = abs (y-y')-}
                     {-in max dx dy-}

unions :: Eq a => [[a]] -> [a]
unions = foldl' union []
