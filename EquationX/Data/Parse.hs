module EquationX.Data.Parse where


import Text.Parsec
import Text.ParserCombinators.Parsec
import EquationX.Data.Equation
import Data.Ratio


parseAtom :: Parser Atom
parseAtom = parseX <|> parseN
  where parseN :: Parser Atom
        parseN = do n <- decimal
                    char '%'
                    m <- decimal
                    return $ N (n%m)

        parseX :: Parser Atom
        parseX = do char 'X' <|> char 'x'
                    return X

decimal :: Parser Integer
decimal = do n <- many1 digit
             let n' = read n
             return n'

parseOp :: Parser a -> Parser (Op a)
parseOp p' = parseAdd p' <|> parseSub p' <|> parseNeg p' <|> parseMul p' <|> parseDiv p'
           where parseAdd :: Parser a -> Parser (Op a)
                 parseAdd p = do a1 <- p
                                 many space
                                 char '+'
                                 many space
                                 a2 <- p
                                 return $ Add a1 a2
                 parseSub :: Parser a -> Parser (Op a)
                 parseSub p = do a1 <- p
                                 many space
                                 char '-'
                                 many space
                                 a2 <- p
                                 return $ Sub a1 a2
                 parseNeg :: Parser a -> Parser (Op a)
                 parseNeg p = do many space
                                 char '-'
                                 a1 <- p
                                 return $ Neg a1
                 parseMul :: Parser a -> Parser (Op a)
                 parseMul p = do a1 <- p
                                 many space
                                 char '*'
                                 many space
                                 a2 <- p
                                 return $ Mul a1 a2
                 parseDiv :: Parser a -> Parser (Op a)
                 parseDiv p = do a1 <- p
                                 many space
                                 char '/'
                                 many space
                                 a2 <- p
                                 return $ Div a1 a2

parseExprTree :: Parser ExprTree
parseExprTree = do a <- parseAtom
                   return $ A a
                <|> do char '('
                       op <- parseOp parseExprTree
                       char ')'
                       return $ O op
