module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
infixl 5 |+|
infixl 5 |-|
infixl 6 |*|

(|+|) :: Term -> Term -> Term
(|+|) leftTerm rightTrem = BinaryTerm Plus leftTerm rightTrem
(|-|) :: Term -> Term -> Term
(|-|) leftTerm rightTrem = BinaryTerm Minus leftTerm rightTrem
(|*|) :: Term -> Term -> Term
(|*|) leftTerm rightTrem = BinaryTerm Times leftTerm rightTrem



-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar = notImplementedYet



-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate eval = case eval of
    BinaryTerm op lhv rhv -> evaluateEval op lhv rhv
    _ -> eval

evaluateEval :: BinaryOp -> Term -> Term -> Term
evaluateEval op lhv rhv =
    let
        left = evaluate lhv
        right = evaluate rhv
    in
        case (op, left, right) of
--            (Plus, IntConstant 0, r) -> r
--            (Plus, l, IntConstant 0) -> l
            (Plus, IntConstant l, IntConstant r) -> IntConstant $ l + r
--            (Minus, l, IntConstant 0) -> l
            (Minus, IntConstant l, IntConstant r) -> IntConstant $ l - r
--            (Times, IntConstant 0, r) -> IntConstant 0
--            (Times, l, IntConstant 0) -> IntConstant 0
--            (Times, IntConstant 1, r) -> r
--            (Times, l, IntConstant 1) -> l
            (Times, IntConstant l, IntConstant r) -> IntConstant $ l * r
            _ -> BinaryTerm op left right