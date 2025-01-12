module Part4.Tasks where

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
  where
    reversed REmpty = []
    reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = foldl (:<) REmpty

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    show REmpty = "[]"
    show rlist = "[" ++ joinElems rlist ++ "]"
      where
        joinElems REmpty = ""
        joinElems (REmpty :< x) = show x
        joinElems (xs :< x) = joinElems xs ++ "," ++ show x

instance Eq a => Eq (ReverseList a) where
    REmpty == REmpty = True
    (as :< a) == (bs :< b) = (a == b) && (as == bs)
    _ == _ = False

instance Semigroup (ReverseList a) where
    (<>) REmpty bs = bs
    (<>) as REmpty = as
    (<>) as (bs :< b) = (as <> bs) :< b

instance Monoid (ReverseList a) where
    mempty = REmpty

instance Functor ReverseList where
    fmap _ REmpty = REmpty
    fmap f (xs :< x) = fmap f xs :< f x

instance Applicative ReverseList where
    pure x  = REmpty :< x
    (<*>) REmpty _ = REmpty
    (<*>) _ REmpty = REmpty 
    (<*>) (fs :< f) as = (fs <*> as) <> (f <$> as)

instance Monad ReverseList where
    (>>=) REmpty _ = REmpty
    (>>=) (xs :< x) f = (xs >>= f) <> f x
