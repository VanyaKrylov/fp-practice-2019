module Task4_1 where

{-
  Задание 4.1
  Реализация монады над функцией.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}
import Data.Char
-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

-- fmap :: (a -> b) -> f a -> f b
-- В качестве аргумента "а" передаемой в fmap ф-ции служит результат выполнения ф-ции,
-- обёрнутой в FunMonad, т.о. для реализации fmap достаточно выполнить композицию ф-ций
-- (a -> b) . (String -> a) = b
-- И на последнем шаге оборачиваем полученное значение обратно в контейнер FunMonad
-- *Task4_1> let f = map(map toUpper)
-- *Task4_1> let fm1 = FunMonad words
-- *Task4_1> f((fun fm1) "Hello World")
-- ["HELLO","WORLD"]
instance Functor FunMonad where
    fmap f (FunMonad fu) = FunMonad (f . fu)

    
instance Applicative FunMonad where
    pure x                           = FunMonad (\s -> x)
    (<*>) (FunMonad mf) (FunMonad m) = FunMonad (\s -> (mf s) $ (m s))
    
instance Monad FunMonad where
    (>>=) (FunMonad m) k = FunMonad (\s -> case m s of ss -> (fun $ k $ ss) s)
{-
(>>=) :: FunMonad (String -> a) 
         -> (a -> FunMonad (String -> b))
         -> FunMonad (String -> b)
         -}
