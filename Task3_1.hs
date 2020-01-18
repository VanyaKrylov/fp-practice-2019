module Task3_1 where

{-
  Задание 3.1
  Числа Пеано: представление чисел на основе нуля и операций "+1" и "-1".
  Необходимо реализовать все классы типов, характерные для целых чисел.
-}

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

x11 = Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))))
x10 = Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))
x2 = Succ (Succ Zero)
y2 = Succ (Succ Zero)
x2' = Pred (Pred Zero)

-- Реализуйте все классы типов, которым должны отвечать целые числа
instance Eq WeirdPeanoNumber where
    Zero   == Zero = True
    Zero   == _    = False
    _      == Zero = False
    Succ a == Succ b = (==) a b
    Pred a == Pred b = (==) a b
    _      == _      = False

instance Ord WeirdPeanoNumber where
    Zero   <= Zero   = True
    Pred _ <= Zero   = True
    Zero   <= Pred _ = False
    Pred a <= Pred b = (<=) a b
    Pred _ <= _      = True
    Succ _ <= Zero   = False
    Zero   <= Succ _ = True
    Succ a <= Succ b = (<=) a b
    Succ _ <= _      = False
    
instance Show WeirdPeanoNumber where
    show Zero   = "Zero"
    show (Succ a) = "(Succ " ++ (show a) ++ ")"
    show (Pred a) = "(Pred " ++ (show a) ++ ")"
    
instance Enum WeirdPeanoNumber where
    toEnum id 
        | id == 0 = Zero
        | id > 0  = consSucc id 0 Zero
        | id < 0  = consPred id 0 Zero
            where
                consSucc x c v
                    | c == x = v
                    | c < x  = consSucc x (c+1) (Succ v)
                    
                consPred x c v
                    | c == x = v
                    | c > x  = consPred x (c-1) (Pred v)
                    
    fromEnum val = 
        case val of
            Zero   -> 0
            Succ a -> 1 + fromEnum a 
            Pred a -> (-1) + fromEnum a

instance Num WeirdPeanoNumber where
    (+) Zero a = a
    (+) a Zero = a
    (+) a b    = toEnum $ fromEnum a + fromEnum b
    
    negate a = toEnum $ (-1) * fromEnum a
    
    (*) a b = toEnum $ fromEnum a * fromEnum b
    
    abs a 
        | a == Zero = a
        | otherwise = negate a
    
    signum a 
        | a == Zero = Zero
        | a >= Zero = Succ Zero
        | otherwise = Pred Zero
    
    fromInteger int = toEnum . fromIntegral $ int 
    








        