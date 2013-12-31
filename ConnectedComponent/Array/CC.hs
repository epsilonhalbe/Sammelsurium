import Debug.Trace

-- import Control.Arrow
import Control.Applicative ((<$>))
import Control.Monad (when)
--import Control.Monad.ST
--import Data.Array.ST
import Data.Array.IO
import Data.Array.MArray
import qualified Data.Set as Set
import Data.List ((\\))
-- import Data.Set (fromAscList,Set (..),singleton, (\\))

type Point = (Int,Int)
type CC = [Point] --CC stands for connected component

main :: IO ()
main = do [imax,jmax] <- map read . words <$> getLine ::IO [Int]
          list <- filter (/='\n')<$> getContents
          arr  <- newListArray ((1,1),(imax,jmax)) list :: IO (IOArray Point Char)
          loop arr (imax,jmax) (1,1) (1,1)
          cc <- length . filter (=='1') <$> getElems arr
          print cc
          return ()

loop :: IOArray Point Char -> (Int, Int) -> Point -> Point -> IO ()
loop arr bd src p = do i <- readArray arr p
                       let src' = next src bd
                       if i == '1'
                         then do writeArray arr p '0'
                                 let nbs = nb p bd
                                 if null nbs
                                   then do writeArray arr src '1' -- we have a singleton or
                                           if src == p && p /= bd
                                             then loop arr bd src' src'
                                             else loop arr bd src src
                                   else mapM_ (loop arr bd src) nbs
                         else when (src == p && p /= bd) $ loop arr bd src' src'

next (x,y) (bx,by) | x+1 > bx && y+1 > by = (bx,by)
                   | x+1 > bx && y+1 < by = (1,y+1)
                   | otherwise            = (x+1,y)


nb :: Point -> (Int, Int) -> [Point]
nb = nb2

nb1 :: Point -> (Int, Int) -> [Point]
nb1 (i,j) (i',j') = let imin = max 1 (i-1)
                        jmin = max 1  (j-1)
                        imax = min i' (i+1)
                        jmax = min j'(j+1)
                    in range ((imin,jmin),(imax,jmax)) \\ [(i,j)]

nb2 :: Point -> (Int, Int) -> [Point]                                            -- |0 0 0
nb2 (x,y) (imax,jmax)= let inRange (i,j) = 1 <= min i j && i <= imax && j <= jmax-- |0 X 1
                       in  filter inRange [(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]  -- |1 1 1

