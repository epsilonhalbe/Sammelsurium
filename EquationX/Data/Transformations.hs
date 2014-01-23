module EquationX.Data.Transformations where

import EquationX.Data.Equation
import Data.Ratio

(|+) :: Equation -> ExprTree -> Equation
(l `EQUALS` r) |+ t = O (Add l t) `EQUALS` O (Add r t)

(|-) :: Equation -> ExprTree -> Equation
(l `EQUALS` r) |- t = O (Sub l t) `EQUALS` O (Sub r t)

(|*) :: Equation -> Ratio Integer-> Equation
(l `EQUALS` r) |* n = O (Mul l (A $ N n)) `EQUALS` O (Mul r (A $ N n))

(|/) :: Equation -> Ratio Integer -> Equation
(l `EQUALS` r) |/ n = O (Div l (A $ N n)) `EQUALS` O (Div r (A $ N n))

xEquals :: Ratio Integer -> Equation
xEquals n = A X `EQUALS` (A $ N n)

simplify :: ExprTree -> ExprTree
-- x -> x*1
simplify   (A X) = O $ Mul (A X) (A (N 1))
-- n -> n
simplify a@(A _) = a
-- + n1 n2 -> n1 + n2
simplify (O (Add (A (N n)) (A(N m)))) = A $ N (n+m)
-- - n1 n2 -> n1 - n2
simplify (O (Sub (A (N n)) (A(N m)))) = A $ N (n-m)
-- - n -> -n
simplify (O (Neg (A (N n))))          = A $ N (negate n)
-- * n1 n2 -> n1 * n2
simplify (O (Mul (A (N n)) (A(N m)))) = A $ N (n*m)
-- / n1 n2 -> n1 / n2
simplify (O (Div (A (N n)) (A(N m)))) = A $ N (n/m)

-- n + x = x + n
simplify (O (Add (A (N n)) (A X)))    = O $ Add (A X) (A (N n))
-- * x n = * n x
simplify (O (Mul (A X) (A (N n))))    = O $ Mul (A (N n)) (A X)
simplify (O (Neg (A X)))              = O $ Mul (A (N (-1))) (A X)
simplify (O (Div (A X) (A (N n))))    = O $ Mul (A (N (1/n))) (A X)

simplify (O (Add (O (Mul (A (N n1)) (A X)))
                 (O (Mul (A (N n2)) (A X)))))= O $ Mul (A (N $ n1+n2)) (A X)
simplify (O (Mul (O (Mul (A (N n1)) (A X)))
                         (A (N n2))        ))= O $ Mul (A (N $ n1*n2)) (A X)

simplify (O (Add (A (N n1))
                 (O (Mul (A (N n2)) (A X)))))= O $ Add (O $ Mul (A (N n2)) (A X)) (A $ N n1)

simplify ( O (Add
                 (O (Add (O (Mul (A (N n1)) (A X))) (A (N n2))))
                 (O (Add (O (Mul (A (N m1)) (A X))) (A (N m2)))))) = O $ Add (O (Mul (A (N $ n1+ m1)) (A X))) (A (N $ n2+m2))

simplify (O (Add x y)) = simplify $ O (Add (simplify x) (simplify y))
simplify (O (Mul x y)) = simplify $ O (Mul (simplify x) (simplify y))
simplify (O (Div x y)) = simplify $ O (Div (simplify x) (simplify y))
-- - x y = + x -y
simplify (O (Sub x y)) = simplify $ O $ Add x (simplify (O (Neg y)))
simplify t = t
