module EquationX.Data.Equation where

import Data.Ratio

data Atom = X | N (Ratio Integer)
instance Show Atom
  where show X     = "X"
        show (N t) = show t

data Op a = Add a a
          | Sub a a
          | Neg a
          | Mul a a
          | Div a a

instance Show a => Show (Op a)
  where show (Add x y) = show x ++" + "++show y
        show (Sub x y) = show x ++" - "++show y
        show (Neg x)   = '-':show x
        show (Mul x y) = show x ++" * "++show y
        show (Div x y) = show x ++" / "++show y

data ExprTree = A Atom
              | O (Op ExprTree)

instance Show ExprTree
  where show (A x)  = show x
        show (O op) = "("++show op++")"

data Equation = EQUALS ExprTree ExprTree


