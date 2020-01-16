module Task2_1 where

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}

import Todo(todo)
import Prelude hiding (lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = EmptyTree
				| Leaf{k :: Integer, val :: v}
				| Node{k :: Integer, val :: v, l :: TreeMap v, r :: TreeMap v}
				deriving(Show,Eq)
-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTree 	_ 	= False
contains Leaf {k = key} k 	= key == k 
contains Node {k=key,l=left, r=right} k
	| k == key = True
	| k > key  = contains right k
	| k < key  = contains left k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup k EmptyTree	  = error "Empty tree"
lookup k (Leaf key val) 
	| key == k  = val
	| otherwise = error "No such key"
lookup k Node {k=key, val=v, l=left, r=right}
	| k == key = v
	| k > key  = lookup k right
	| k < key  = lookup k left

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) EmptyTree = Leaf k v
insert (k, v) (Leaf key val)
	| k == key = Leaf k v
	| k > key  = Node key val EmptyTree (Leaf k v) 
	| k < key  = Node key val (Leaf k v) EmptyTree
insert nv@(k, v) Node {k=key, val=val, l=left, r=right}
	| k == key = Node k v left right 
	| k > key  = Node key val left (insert nv right)
	| k < key  = Node key val (insert nv left) right
	
-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove k EmptyTree = EmptyTree
remove k lea@(Leaf key val) 
	| k == key  = emptyTree
	| otherwise = lea
remove k node@(Node key val ln rn) 
	| k == key = 
		case (ln, rn) of
			(ln, EmptyTree) -> ln
			(_, _)			-> (Node nkey nval ln (remove nkey rn))
				where
					minK = foldr1 min . map fst . listFromTree $ rn
					nkey = minK
					nval = lookup minK rn
				
	| k > key  =
		case (ln, rn) of
			(EmptyTree, lf@Leaf{k = lkey}) -> if lkey == k 
											  then Leaf key val
											  else lf
			(_		  , _				 ) -> node{r = remove k rn}
	| k < key  =
		case (ln, rn) of
			(lf@Leaf{k = lkey}, EmptyTree) -> if lkey == k 
											  then Leaf key val
											  else lf
			(_				  , _		 ) -> node{l = remove k ln}
remove _ _     = undefined

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i t = todo

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert EmptyTree lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree EmptyTree        = []
listFromTree (Leaf k val)     = [(k,val)]
listFromTree (Node k val l r) = [(k,val)] ++ (listFromTree l) ++ (listFromTree r)

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = todo

lt = Leaf 1 7.22
lt2 = Leaf 2 "2.22"
lt3 = Leaf 3 "3.22"
lt4 = Leaf 4 "4.22"
lt5 = Leaf 5 "5.22"
lt6 = Leaf 6 "6.22"
n5 = Node 5 "5.22" lt4 lt6 
n3 = Node 3 "3.22" lt2 n5 
n1 = Node 1 "1.22" emptyTree n3 