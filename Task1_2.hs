module Task1_2 where

{-
  Задание 1.2
  Необходимо реализовать четыре любые функции в данном файле
-}
import Prelude hiding (pow, gcd)
import Todo(todo)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x 0 = x
gcd x y = let 
              smaller = min x y
              bigger  = max x y
          in 
              gcd smaller $ mod bigger smaller

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = solut from to 1
    where 
        solut from to toggle 
          | toggle >= from && toggle < to = True
          | toggle < from                 = solut from to ((toggle+1)^2)
          | toggle >= to                  = False

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = todo

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x 1 = x
pow x y = x * pow x (y-1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = solut x (x-1)
    where 
        solut x 1 = True 
        solut x d 
            | mod x d == 0 = False
            | otherwise    = solut x (d-1)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
