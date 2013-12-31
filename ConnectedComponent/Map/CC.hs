import Debug.Trace

import Control.Arrow
import Control.Applicative ((<$>))
import qualified Data.Map as M
import Data.Ix

type Point = (Int,Int)
type CC = [Point] --CC stands for connected component

main :: IO ()
main = do [xmax,ymax] <- map read . words <$> getLine ::IO [Int]
          let indices = range ((1,1),(xmax,ymax))
          assocs <- zip indices . filter (/='\n')<$> getContents
          let ones = filter (('1'==).snd) assocs
              arr = M.fromAscList assocs
          return ()

nb :: Point -> (Int,Int) -> [Point]
--nb (x,y) (imax,jmax) = let inRange (i,j) = 1 <= min i j && i <= imax && j <= jmax
--                       in filter inRange range ((x-1,y-1),(x+1,y+1)
--
nb (i,j) (i',j') = let imin = max 1 (i-1)
                       jmin = max 1  (j-1)
                       imax = min i' (i+1)
                       jmax = min j'(j+1)
                   in range ((imin,jmin),(imax,jmax))\\ [(i,j)]

nb2 :: Point -> (Int, Int) -> [Point]                                            -- |0 0 0
nb2 (x,y) (imax,jmax)= let inRange (i,j) = 1 <= min i j && i <= imax && j <= jmax-- |0 X 1
                       in  filter inRange [(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]  -- |1 1 1


calculateCC :: [CC] -> CC -> [Point] -> Map Point Char -> [CC]
calculateCC ccs cc nbs base    = undefined
calculateCC ccs cc nbs M.empty = (nbs++cc)::ccs
calculateCC ccs [] [] (b:base) = calculateCC ccs [] [b] base
calculateCC ccs cc [] (b:base) = calculateCC (cc:ccs) [] [b] base
calculateCC ccs cc nbs base = let nb x = filter (\b -> dist x b == 1) base
                                  nbs'  = unions (map nb nbs) \\ nbs
                                  base' = base \\ nbs'
                              in calculateCC ccs (nbs++cc) nbs' base'

