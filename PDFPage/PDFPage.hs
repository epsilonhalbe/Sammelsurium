{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import qualified Data.Text as T
import Data.Char (toLower)
import System.Environment (getArgs)


default (T.Text)

main :: IO ()
main = do a <- getArgs
          if not $ null a
            then addPages (T.pack $head a)
            else do putStrLn "Do you want to edit ALL pdfs in this folder??"
                    putStrLn "Type 'Y' or 'y' and <Enter> to confirm!"
                    putStrLn "Everything else cancels your actions."
                    putStrLn "Be sure to type the right letter."
                    c <- toLower <$> getChar
                    if c /= 'y'
                      then do putStrLn "4 -- 3 -- 2 --"
                              putStrLn "Abort Nuclear Missile Launch!"
                              putStrLn "Sigh! Nothing Happened. Everyone is happy!"
                      else do pdfs <- shelly $ map toTextIgnore . filter isPDF <$> ls "." :: IO [T.Text]
                              mapM_ addPages pdfs
    where isPDF :: Shelly.FilePath -> Bool
          isPDF x = or [hasExt (T.pack [p,d,f]) x | p<-"Pp", d<-"Dd", f<-"Ff"]


addPages :: T.Text -> IO ()
addPages arg = do let n = T.length arg
                  info  <- shelly $ run "pdfinfo" [arg]
                  let pages = last . head . filter ((=="Pages:").head) . map T.words $ T.lines info
                      (ar,g)= T.splitAt (n-4) arg
                  shelly $ mv (fromText arg) (fromText $ ar `T.append` "_" `T.append` pages `T.append` g)
