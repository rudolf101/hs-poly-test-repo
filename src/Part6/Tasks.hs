{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map (Map, fromList, empty, (!?), findWithDefault, toList)

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
    zeroImpl :: Int -> Int -> mx
    eyeImpl ::  Int -> mx
    multiplyImpl :: mx -> mx -> mx
    determinantImpl :: mx -> Int

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
    zeroImpl _ _ = 0
    eyeImpl _ = 1
    multiplyImpl x y = x * y
    determinantImpl x = x

instance Matrix [[Int]] where
    zeroImpl cols rows = replicate rows (replicate cols 0)
    eyeImpl size = [ [if i == j then 1 else 0 | j <- [0..size-1]] | i <- [0..size-1] ]
    multiplyImpl a b =
        let transB = transpose b
        in [[sum (zipWith (*) row col) | col <- transB] | row <- a]
      where
        transpose [] = []
        transpose ([]:_) = []
        transpose x = map head x : transpose (map tail x)
    determinantImpl [[x]] = x
    determinantImpl mtx = sum [((-1) ^ col) * head mtx !! col * determinantImpl (minor mtx col) | col <- [0..length mtx - 1]]
      where
        minor m i = map (removeAt i) (tail m)
        removeAt i xs = take i xs ++ drop (i + 1) xs

instance Matrix (SparseMatrix Int) where
    zeroImpl cols rows = SparseMatrix cols rows empty
    eyeImpl size = SparseMatrix size size $ fromList [((i, i), 1) | i <- [0..size-1]]
    multiplyImpl (SparseMatrix w1 h1 elems1) (SparseMatrix w2 h2 elems2)
        | w1 /= h2 = error "Размеры матриц не совпадают для умножения"
        | otherwise = SparseMatrix w2 h1 $ fromList
            [ ((r, c), sum [findWithDefault 0 (r, k) elems1 * findWithDefault 0 (k, c) elems2 | k <- [0..w1-1]])
            | r <- [0..h1-1], c <- [0..w2-1], let v = sum [findWithDefault 0 (r, k) elems1 * findWithDefault 0 (k, c) elems2 | k <- [0..w1-1]], v /= 0]
    determinantImpl (SparseMatrix w h elems)
        | w /= h = error "Матрица не квадратная"
        | w == 1 = findWithDefault 0 (0, 0) elems
        | otherwise = sum [((-1) ^ col) * findWithDefault 0 (0, col) elems * determinantImpl (minorSparse col) | col <- [0..w-1]]
      where
        minorSparse col = SparseMatrix (w - 1) (h - 1) $ fromList
            [ ((r - 1, c - (if c > col then 1 else 0)), v)
            | ((r, c), v) <- toList elems, r > 0, c /= col ]

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye = eyeImpl
-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero = zeroImpl
-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix = multiplyImpl

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant = determinantImpl
