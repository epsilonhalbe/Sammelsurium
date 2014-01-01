module Main where

import Control.Applicative ((<$>))
import System.FilePath.Posix (takeExtension)
import Control.Arrow ((***))
import Data.Char (toLower)
import qualified Data.Map as M

type MIME = String
type Ext = String

show' :: Maybe String -> String
show' (Just x) = x
show' Nothing = "UNKNOWN"

main :: IO ()
main = do n <- read <$> getLine :: IO Int
          _ <- read <$> getLine :: IO Int
          (mime, filenames) <-  splitAt n .lines <$> getContents :: IO ([String], [String])
          let mime' = M.fromList $ map ((map toLower *** tail) . break (==' ')) mime :: M.Map Ext MIME
              fileexts = map (show' . (`M.lookup` mime') . map toLower . drop 1 . takeExtension) filenames
          mapM_ putStrLn fileexts

