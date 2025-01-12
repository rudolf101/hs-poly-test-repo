module Part5.Tasks where

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldl (\acc x -> acc ++ [f x]) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldl (\acc x -> acc ++ f x) []

myConcat :: [[a]] -> [a]
myConcat = myFoldl (\acc xs -> acc ++ xs) []

myReverse :: [a] -> [a]
myReverse = myFoldl (\acc x -> x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldl (\acc x -> if p x then acc ++ [x] else acc) []

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldl (\(ts, fs) x -> if p x then (ts ++ [x], fs) else (ts, fs ++ [x])) ([], [])
