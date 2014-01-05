import Control.Applicative
import Data.Attoparsec
import qualified Data.ByteString as B

import PXMParser

main :: IO ()
main = do bs <- B.getContents
          let pxm = parseOnly parsePXM bs
          case pxm of Right x  -> print x
                      Left msg -> putStrLn msg

