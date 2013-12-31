import Control.Applicative
import Data.List (foldl', (\\), sort)

main :: IO ()
main = do n <- read <$> getLine :: IO Int
          (input,lastline) <- splitAt n . lines <$> getContents
          let lastline' = sort $ head lastline
              input' = filter (`isMadeOf` lastline') input
              out = fst $ foldl' maxStrVal ("",0) input'
          putStrLn out
          {-putStrLn $ head lastline-}
          return()


scrabbleValues :: Char -> Int
scrabbleValues x | x ∈ "eaionrtlsu" = 1
                 | x ∈ "dg"         = 2
                 | x ∈ "bcmp"       = 3
                 | x ∈ "fhvwy"      = 4
                 | x ∈ "k"          = 5
                 | x ∈ "jx"         = 8
                 | x ∈ "qz"         = 10
                 | otherwise        = 0

maxStrVal :: (String, Int) -> String -> (String, Int)
maxStrVal x@(_,n) str' = let n' = foldl' (\a b -> a + scrabbleValues b) 0 str'
                         in if n < n'
                              then (str', n')
                              else x

(∈) :: Eq a => a -> [a] -> Bool
(∈) = elem

isMadeOf :: (Eq a) => [a] -> [a] -> Bool
[] `isMadeOf` _ = True
(w:rd) `isMadeOf` letters = let (letters', bool) = reduceBag w letters
                            in bool && (rd `isMadeOf` letters')

reduceBag :: (Eq a) => a -> [a] -> ([a], Bool)
reduceBag x bag = let inside = x ∈ bag
                  in if inside then (bag \\ [x], inside)
                               else (bag, inside)
