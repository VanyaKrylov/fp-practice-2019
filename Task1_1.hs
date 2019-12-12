module Task1_1 where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

import Todo(todo)

data Op = Add | Subs | Mul
            deriving(Show,Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ op :: Op, lhv :: Term, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)


-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
infixl 6 |+|
infixl 6 |-|
infixl 7 |*|

(|+|) :: Term -> Term -> Term
(|+|) (IntConstant l) (IntConstant r) = IntConstant (l + r)
(|+|) l r = BinaryTerm Add l r

(|-|) :: Term -> Term -> Term
(|-|) (IntConstant l) (IntConstant r) = IntConstant (l - r)
(|-|) l r = BinaryTerm Subs l r

(|*|) :: Term -> Term -> Term
(|*|) (IntConstant l) (IntConstant r) = IntConstant (l * r)
(|*|) l r = BinaryTerm Mul l r

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = 
    case expression of 
         (Variable name)      -> if varName == name 
                                 then replacement
                                 else (Variable name)
         (BinaryTerm o l r)   -> BinaryTerm (o) (replaceVar varName replacement l) (replaceVar varName replacement r) 
         (IntConstant val)    -> IntConstant val

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (IntConstant val) = IntConstant val
evaluate BinaryTerm {op = oper, lhv = l, rhv = r} = 
    case oper of
         Add  -> evaluate l |+| evaluate r
         Subs -> evaluate l |-| evaluate r
         Mul  -> evaluate l |*| evaluate r
evaluate _ = undefined
