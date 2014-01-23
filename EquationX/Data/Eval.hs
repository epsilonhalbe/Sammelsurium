module EquationX.Data.Eval where

import EquationX.Data.Equation
import Data.Ratio

class Eval a where
  eval :: a -> Ratio Integer -> Ratio Integer

instance Eval Atom
  where eval X ω     = ω
        eval (N n) _ = n

instance (Eval a) => Eval (Op a)
  where eval (x `Add` y) ω = eval x ω + eval y ω
        eval (x `Sub` y) ω = eval x ω - eval y ω
        eval (Neg x) ω     = negate (eval x ω)
        eval (x `Mul` y) ω = eval x ω * eval y ω
        eval (x `Div` y) ω = eval x ω / eval y ω

instance Eval ExprTree
  where eval (A x) = eval x
        eval (O x) = eval x
