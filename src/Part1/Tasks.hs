module Part1.Tasks where

import Util(notImplementedYet)
import Data.Fixed (mod')

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = hSin 0.0 16 (x `mod'` (2 * pi))
  where
    hSin acc 0 x = acc + x
    hSin acc n x = hSin (acc + ((-1) ** fromIntegral n / fromIntegral (hFact (2 * n + 1)) * x ** (2 * fromIntegral n + 1))) (n - 1) x
    hFact 0 = 1
    hFact n = product [1..n]

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = hCos 0.0 16 (x `mod'` (2 * pi))
  where
    hCos acc 0 _ = acc + 1.0
    hCos acc n x = hCos (acc + ((-1) ** fromIntegral n / fromIntegral (hFact (2 * n)) * x ** (2 * fromIntegral n))) (n - 1) x
    hFact 0 = 1
    hFact n = product [1..n]


-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect dd mm yy
    | mm < 1 || mm > 12 = False
    | dd < 1 || dd > daysInMonth = False
    | otherwise = True
  where
    isLeapYear = (yy `mod` 4 == 0 && yy `mod` 100 /= 0) || (yy `mod` 400 == 0)
    daysInMonth
      | mm == 2 = if isLeapYear then 29 else 28
      | mm `elem` [4, 6, 9, 11] = 30
      | otherwise = 31


-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow _ 0 = 1
myPow base exp = base * myPow base (exp - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n
  | n < 2 = False
  | otherwise = all (\x -> n `mod` x /= 0) [2..floor . sqrt $ fromIntegral n]

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = abs $ 0.5 * sum [(x1 * y2 - y1 * x2) | ((x1, y1), (x2, y2)) <- zip points (tail points ++ [head points])]

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
  | not (a + b > c && a + c > b && b + c > a) = -1 -- импостер
  | a2 + b2 == c2 || a2 + c2 == b2 || b2 + c2 == a2 = 2  -- прямоугольный
  | a2 + b2 < c2 || a2 + c2 < b2 || b2 + c2 < a2 = 0   -- тупоугольный
  | otherwise = 1  -- остроугольный
  where
    a2 = a * a
    b2 = b * b
    c2 = c * c
