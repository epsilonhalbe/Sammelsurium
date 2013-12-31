import Control.Applicative
import Data.Attoparsec
import qualified Data.ByteString as B

import PXMParser

main :: IO ()
main = do bs <- B.getContents
          let pxm = parseOnly parsePXM bs
          case pxm of Right _  -> putStrLn "OK"
                      Left msg -> putStrLn msg

