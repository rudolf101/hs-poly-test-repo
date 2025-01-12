module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving (Show, Eq)

-- задаем ассоциативность и приоритеты операций
infixl 7 |+|, |-|
infixl 8 |*|

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus

(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus

(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (Variable v)
  | varName == v = replacement
  | otherwise = Variable v
replaceVar _ _ term@(IntConstant _) = term
replaceVar varName replacement (BinaryTerm op t1 t2) =
  BinaryTerm op (replaceVar varName replacement t1) (replaceVar varName replacement t2)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (IntConstant x) = IntConstant x
evaluate (Variable v) = Variable v  -- переменную нельзя вычислить
evaluate (BinaryTerm op t1 t2) =
  case (op, evaluate t1, evaluate t2) of
    (Plus, IntConstant x, IntConstant y)  -> IntConstant (x + y)
    (Minus, IntConstant x, IntConstant y) -> IntConstant (x - y)
    (Times, IntConstant x, IntConstant y) -> IntConstant (x * y)
    _ -> BinaryTerm op (evaluate t1) (evaluate t2)  -- возвращаем упрощенное выражение, если это не константы
