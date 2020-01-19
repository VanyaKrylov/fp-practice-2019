module Task3_2 where

{-
  Задание 3.2
  Структура данных "перевёрнутый список" представляет собой зеркально обращённый
  односвязный список из стандартной библиотеки, для которого взятие последнего
  элемента является наиболее эффективной операцией.
  Необходимо реализовать все функции и классы типов в данном файле.
-}

import Todo(todo)

data ReverseList a = RNil | RCons { list::(ReverseList a), value::a} 

rl  = RCons (RCons (RCons RNil 3) 2) 1
rl2 = RCons (RCons RNil (-3)) (-2)

infixl 6 !+!

(!+!) :: ReverseList a -> ReverseList a -> ReverseList a
(!+!) RNil b = b
(!+!) a b    = (list a) !+! RCons b (value a)

rlistToList :: ReverseList a -> [a]
rlistToList RNil = []
rlistToList (RCons l v) = (rlistToList l) ++ [v]

listToRList :: [a] -> ReverseList a
listToRList []     = RNil
listToRList m      = func (reverse m)
    where
        func []     = RNil
        func (x:xs) = RCons (func xs) x

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor
instance Show a => Show (ReverseList a) where
    show RNil            = "[]"
    show ini@(RCons lst val) = prnt ini 0
        where 
            prnt RNil _ = "]"
            prnt l c
                | c == 0    = 
                    case l of
                        RNil           -> "[" ++ (show val ) ++ "]"
                        (RCons RNil v) -> "[" ++ (show v) ++ "]"
                        (RCons ls v)   -> "[" ++ (show v) ++ "," ++ (prnt ls (c+1))
                | otherwise =
                    case l of
                        RNil           -> (show val ) ++ "]"
                        (RCons RNil v) -> (show v) ++ "]"
                        (RCons ls v)   -> (show v) ++ "," ++ (prnt ls (c))
                        
instance Eq a => Eq (ReverseList a) where
    (==) RNil RNil                   = True
    (==) (RCons l1 v1) (RCons l2 v2) = v1 == v2 && l1 == l2
    (==) _    _                      = False
    
instance Ord a => Ord (ReverseList a) where
    (<=) RNil _    = True
    (<=) _    RNil = False
    (<=) l1   l2   
        | value l1 == value l2 = (<=) (list l1) (list l2)
        | value l1 > value l2  = False
        | otherwise            = True

instance Semigroup (ReverseList a) where
    (<>) a b = a !+! b
      
instance Monoid (ReverseList a) where
    mempty = RNil
    
instance Functor ReverseList where
    fmap f RNil        = RNil
    fmap f (RCons l v) = RCons (fmap f l) (f v) 