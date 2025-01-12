module Part3.Tasks where

import Util (notImplementedYet)
import Data.Char (digitToInt)
import Data.List (group, sort, nub, maximumBy)
import Data.Ord (comparing)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
-- finc f x = f x : finc f (x + 1)
-- Альтернативная версия:
finc f x = [f n | n <- [x..]]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq numbers = fst $ maximumBy (comparing snd) frequency
  where
    digits = concatMap (map digitToInt . show . abs) numbers  -- преобразуем числа в список цифр
    groupedDigits = group $ sort digits  -- группируем одинаковые цифры
    frequency = [(head g, length g) | g <- groupedDigits]  -- подсчитываем частоту каждой цифры

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x:xs) = foldl (\acc y -> if y `elem` acc then acc else acc ++ [y]) [x] xs

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f lst = foldr (\x acc -> insertGrouped (f x) x acc) [] lst
  where
    insertGrouped k v [] = [(k, [v])]
    insertGrouped k v ((k', vs):rest)
      | k == k' = (k', v : vs) : rest  -- добавляем элемент в существующую группу
      | otherwise = (k', vs) : insertGrouped k v rest  -- добавляем новую группу, если ключ новый
