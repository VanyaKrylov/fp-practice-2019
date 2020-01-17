module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f i []     = i
foldl f i [x]   = f i x
foldl f i (x:xs) = foldl f i xs `f` x

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f i []     = i
foldr f i [x]    = f x i
foldr f i (x:xs) = x `f` foldr f i xs

unfoldr :: (b -> Maybe(a, b)) -> b -> [a]
unfoldr f i = 
    case f i of
        Nothing   -> []
        Just(x,y) -> x : unfoldr f y

unfoldrInf :: (b -> (a, b)) -> b -> [a]
unfoldrInf f i = [el] ++ (unfoldrInf f base) 
    where (el,base) = f i 

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f lst = foldr ((:) . f) [] lst 

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product = foldr (*) 1

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes = foldl func [] 
	where
		func x1 x2 = 
			case x2 of 
				Just val -> val : x1
				Nothing -> x1

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal = todo

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot = todo

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem = todo

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = todo

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append = todo

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = todo
